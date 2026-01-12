;;;; streaming.lisp - Server-side streaming RPC support
;;;;
;;;; Implements server streaming, client streaming, and bidirectional streaming

(in-package #:clgrpc.server)

;;; Server Stream State

(defstruct grpc-server-stream
  "Represents a streaming gRPC call on the server side"
  (connection nil :type (or null http2-connection))
  (stream-id nil :type fixnum)
  (headers nil :type list)

  ;; Stream state
  (recv-closed nil :type boolean)      ; Have we received END_STREAM?
  (send-closed nil :type boolean)      ; Have we sent END_STREAM?

  ;; Message queues
  (request-queue nil)                   ; Queue of received messages
  (lock (bordeaux-threads:make-lock "server-stream-lock"))
  (recv-cv (bordeaux-threads:make-condition-variable :name "server-stream-recv-cv"))
  (send-lock (bordeaux-threads:make-lock "server-stream-send-lock")))

;;; Simple FIFO queue for received messages (same as client)

(defun make-queue ()
  "Create a simple queue for messages."
  (cons nil nil))  ; (head . tail)

(defun queue-push (queue item)
  "Add item to end of queue."
  (let ((new-cell (cons item nil)))
    (if (null (car queue))
        ;; Empty queue
        (setf (car queue) new-cell
              (cdr queue) new-cell)
        ;; Non-empty queue
        (progn
          (setf (cdr (cdr queue)) new-cell)
          (setf (cdr queue) new-cell)))))

(defun queue-pop (queue)
  "Remove and return item from front of queue. Returns NIL if empty."
  (when (car queue)
    (let ((item (car (car queue))))
      (setf (car queue) (cdr (car queue)))
      (when (null (car queue))
        (setf (cdr queue) nil))
      item)))

(defun queue-empty-p (queue)
  "Check if queue is empty."
  (null (car queue)))

;;; Thread-Safe Queue for Bidirectional Streaming

(defstruct bidi-queue
  "Thread-safe blocking queue for bidirectional streaming message passing"
  (items (cons nil nil))  ; Simple queue: (head . tail)
  (lock (bordeaux-threads:make-lock "bidi-queue-lock"))
  (cv (bordeaux-threads:make-condition-variable :name "bidi-queue-cv")))

(defun bidi-queue-push (queue item)
  "Add item to queue and notify waiting threads."
  (bordeaux-threads:with-lock-held ((bidi-queue-lock queue))
    (queue-push (bidi-queue-items queue) item)
    (bordeaux-threads:condition-notify (bidi-queue-cv queue))))

(defun bidi-queue-pop (queue timeout-ms receiver-done-fn)
  "Remove and return item from queue.

   Blocks until:
   - An item is available, OR
   - timeout-ms expires (if provided), OR
   - receiver-done-fn returns true (stream closed)

   Args:
     queue: bidi-queue
     timeout-ms: Timeout in milliseconds (or nil for no timeout)
     receiver-done-fn: Function that returns true when receiver is done

   Returns:
     Message bytes, or NIL if timeout/closed"
  (let ((start-time (get-internal-real-time))
        (timeout-internal (when timeout-ms
                           (* timeout-ms (/ internal-time-units-per-second 1000)))))
    (bordeaux-threads:with-lock-held ((bidi-queue-lock queue))
      (loop
        ;; Try to get item
        (let ((item (queue-pop (bidi-queue-items queue))))
          (when item
            (return item)))

        ;; Check if receiver is done and queue is empty
        (when (and (funcall receiver-done-fn) (queue-empty-p (bidi-queue-items queue)))
          (return nil))

        ;; Check timeout
        (when timeout-internal
          (let ((elapsed (- (get-internal-real-time) start-time)))
            (when (>= elapsed timeout-internal)
              (return nil))))

        ;; Wait for item or timeout
        (let ((remaining-time (when timeout-internal
                               (- timeout-internal
                                  (- (get-internal-real-time) start-time)))))
          (bordeaux-threads:condition-wait
           (bidi-queue-cv queue)
           (bidi-queue-lock queue)
           :timeout (when remaining-time
                     (/ remaining-time internal-time-units-per-second))))))))

;;; Bidirectional Streaming Helper Macro

(defmacro with-bidirectional-stream ((send-fn recv-fn) context &body body)
  "Enable true concurrent bidirectional streaming in a handler.

   This macro spawns a background receiver thread, allowing your handler to
   send and receive messages independently without blocking.

   Args:
     send-fn: Symbol for the send function (msg-bytes -> nil)
     recv-fn: Symbol for the receive function (&optional timeout-ms -> msg-bytes or nil)
     context: The handler context (must have grpc-stream in it)
     body: Handler code that uses send-fn and recv-fn

   The recv-fn takes an optional timeout in milliseconds. It returns:
     - Message bytes if received
     - NIL if timeout expires or stream is closed

   The send-fn sends a message immediately (non-blocking).

   Example:
     (defgrpc-method route-chat ((service route-guide-service)
                                 (request route-note)
                                 context)
       (:rpc-type :bidirectional)
       (declare (ignore service request))

       (with-bidirectional-stream (send-note recv-note) context
         ;; Can send and receive concurrently!
         (loop for note-bytes = (recv-note 100)  ; 100ms timeout
               while note-bytes
               do (let ((note (proto-deserialize 'route-note note-bytes)))
                    (when-let ((reply (process-note note)))
                      (send-note (proto-serialize reply))))))

       (values +grpc-status-ok+ nil nil))"
  (let ((stream-var (gensym "STREAM"))
        (recv-queue (gensym "RECV-QUEUE"))
        (receiver-done (gensym "RECEIVER-DONE"))
        (receiver-error (gensym "RECEIVER-ERROR"))
        (receiver-thread (gensym "RECEIVER-THREAD")))
    `(let* ((,stream-var (get-stream ,context))
            (,recv-queue (make-bidi-queue))
            (,receiver-done nil)
            (,receiver-error nil)
            (,receiver-thread
             (bordeaux-threads:make-thread
              (lambda ()
                (handler-case
                    (loop for msg-bytes = (server-stream-recv ,stream-var)
                          while msg-bytes
                          do (bidi-queue-push ,recv-queue msg-bytes))
                  (error (e)
                    (setf ,receiver-error e)))
                (setf ,receiver-done t))
              :name "bidi-receiver")))
       (unwind-protect
           (flet ((,send-fn (msg-bytes)
                    (when ,receiver-error
                      (error "Receiver thread error: ~A" ,receiver-error))
                    (server-stream-send ,stream-var msg-bytes))
                  (,recv-fn (&optional (timeout-ms nil))
                    (when ,receiver-error
                      (error "Receiver thread error: ~A" ,receiver-error))
                    (bidi-queue-pop ,recv-queue timeout-ms (lambda () ,receiver-done))))
             ,@body)
         ;; Cleanup: wait for receiver thread to finish
         (when (bordeaux-threads:thread-alive-p ,receiver-thread)
           (bordeaux-threads:join-thread ,receiver-thread :timeout 5))))))

;;; Stream Operations (called by handlers)

(defun server-stream-recv (stream &key (timeout-ms nil))
  "Receive next message from client.

   Returns:
     Message bytes, or NIL if stream is closed

   Blocks until a message is available or stream ends."
  (let ((start-time (get-internal-real-time))
        (timeout-internal (when timeout-ms
                           (* timeout-ms (/ internal-time-units-per-second 1000)))))

    (bordeaux-threads:with-lock-held ((grpc-server-stream-lock stream))
      (loop
        ;; Check if we have a message
        (unless (queue-empty-p (grpc-server-stream-request-queue stream))
          (return (queue-pop (grpc-server-stream-request-queue stream))))

        ;; Check if stream is closed (no more messages coming)
        (when (grpc-server-stream-recv-closed stream)
          (return nil))

        ;; Check timeout
        (when timeout-internal
          (let ((elapsed (- (get-internal-real-time) start-time)))
            (when (>= elapsed timeout-internal)
              (return nil))))

        ;; Wait for message or close
        (let ((remaining-time (when timeout-internal
                               (- timeout-internal
                                  (- (get-internal-real-time) start-time)))))
          (bordeaux-threads:condition-wait
           (grpc-server-stream-recv-cv stream)
           (grpc-server-stream-lock stream)
           :timeout (when remaining-time
                     (/ remaining-time internal-time-units-per-second))))))))

(defun server-stream-send (stream message-bytes)
  "Send a response message to client.

   Args:
     stream: grpc-server-stream
     message-bytes: Serialized protobuf message

   This does NOT close the stream - server will send trailers when handler returns."
  (bordeaux-threads:with-lock-held ((grpc-server-stream-send-lock stream))
    (when (grpc-server-stream-send-closed stream)
      (error "Stream send side already closed"))

    ;; Send DATA frame without END_STREAM
    (let* ((conn (grpc-server-stream-connection stream))
           (stream-id (grpc-server-stream-stream-id stream))
           (grpc-message (encode-grpc-message message-bytes)))

      ;; Need connection write lock for socket writes
      ;; NOTE: Flush happens OUTSIDE lock to avoid blocking frame reader's ACKs
      (bt:with-lock-held ((http2-connection-write-lock conn))
        (let ((data-frame (make-http2-frame
                           :length (length grpc-message)
                           :type +frame-type-data+
                           :flags 0  ; No END_STREAM
                           :stream-id stream-id
                           :payload grpc-message)))
          (write-frame-to-stream data-frame (http2-connection-socket conn) :flush nil)))

      ;; Flush outside the write lock
      (clgrpc.transport:buffered-flush (http2-connection-socket conn)))))

;;; Internal: Called by server frame handlers to add messages to queue

(defun server-stream-add-message (stream message-bytes)
  "Add a received message to the stream's queue.

   Called internally by server DATA frame handler."
  (bordeaux-threads:with-lock-held ((grpc-server-stream-lock stream))
    (queue-push (grpc-server-stream-request-queue stream) message-bytes)
    (bordeaux-threads:condition-notify (grpc-server-stream-recv-cv stream))))

(defun server-stream-mark-recv-closed (stream)
  "Mark that we've received END_STREAM (no more messages from client).

   Called internally by server DATA frame handler."
  (bordeaux-threads:with-lock-held ((grpc-server-stream-lock stream))
    (setf (grpc-server-stream-recv-closed stream) t)
    (bordeaux-threads:condition-notify (grpc-server-stream-recv-cv stream))))

(defun server-stream-close (stream status-code status-message response-metadata)
  "Close the stream by sending trailers with status.

   Called internally by server after handler returns.

   Args:
     stream: grpc-server-stream
     status-code: gRPC status code
     status-message: Status message (or nil)
     response-metadata: Response metadata (or nil)"
  (bordeaux-threads:with-lock-held ((grpc-server-stream-send-lock stream))
    (when (grpc-server-stream-send-closed stream)
      (return-from server-stream-close nil))

    (let* ((conn (grpc-server-stream-connection stream))
           (stream-id (grpc-server-stream-stream-id stream)))

      ;; CRITICAL: Need connection write lock for HPACK encoder and socket writes
      ;; NOTE: Flush happens OUTSIDE lock to avoid blocking frame reader's ACKs
      (bt:with-lock-held ((http2-connection-write-lock conn))
        (let ((encoder-ctx (http2-connection-hpack-encoder conn)))
          ;; Send trailers with status
          (let ((trailers (encode-grpc-trailers status-code status-message)))
            (let ((trailers-bytes (hpack-encode-headers encoder-ctx trailers)))
              (let ((trailers-frame (make-http2-frame
                                     :length (length trailers-bytes)
                                     :type +frame-type-headers+
                                     :flags (logior +flag-end-headers+ +flag-end-stream+)
                                     :stream-id stream-id
                                     :payload trailers-bytes)))
                (write-frame-to-stream trailers-frame (http2-connection-socket conn) :flush nil))))))

      ;; Flush outside the write lock
      (clgrpc.transport:buffered-flush (http2-connection-socket conn))

      (setf (grpc-server-stream-send-closed stream) t))))
