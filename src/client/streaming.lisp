;;;; streaming.lisp - Streaming RPC support
;;;;
;;;; Implements client streaming, server streaming, and bidirectional streaming

(in-package #:clgrpc.client)

;;; Streaming Call State

(defstruct grpc-stream
  "Represents a streaming gRPC call (client/server/bidirectional)"
  (connection nil :type (or null http2-connection))
  (stream-id nil :type (or null fixnum))
  (service "" :type string)
  (method "" :type string)
  (authority "" :type string)
  (timeout nil :type (or null fixnum))
  (metadata nil :type list)

  ;; Stream state
  (headers-sent nil :type boolean)      ; Have we sent HEADERS frame?
  (send-closed nil :type boolean)       ; Have we sent END_STREAM?
  (recv-closed nil :type boolean)       ; Have we received END_STREAM?

  ;; Response state
  (response-headers nil :type (or null list))
  (response-queue nil)                   ; Queue of received messages
  (response-trailers nil :type (or null list))
  (status nil :type (or null fixnum))
  (status-message nil :type (or null string))
  (error nil :type (or null condition))

  ;; Synchronization
  (lock (bordeaux-threads:make-lock "grpc-stream-lock"))
  (recv-cv (bordeaux-threads:make-condition-variable :name "stream-recv-cv"))
  (send-lock (bordeaux-threads:make-lock "stream-send-lock")))

;;; Stream Creation

(defun create-stream (connection service method &key authority timeout metadata)
  "Create a new streaming gRPC call.

   This doesn't send anything yet - call stream-start to begin."
  (make-grpc-stream
   :connection connection
   :service service
   :method method
   :authority authority
   :timeout timeout
   :metadata metadata
   :response-queue (make-queue)))

;;; Simple FIFO queue for received messages

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

;;; Stream Operations

(defun stream-start (stream)
  "Start the stream by sending HEADERS frame.

   Must be called before sending messages."
  (bordeaux-threads:with-lock-held ((grpc-stream-send-lock stream))
    (when (grpc-stream-headers-sent stream)
      (error "Stream already started"))

    (let* ((conn (grpc-stream-connection stream))
           (stream-id (http2-connection-next-stream-id conn))
           (hpack-ctx (http2-connection-hpack-encoder conn)))

      ;; Wait for connection ready
      (bt:with-lock-held ((http2-connection-ready-lock conn))
        (loop until (http2-connection-settings-received conn)
              do (bt:condition-wait (http2-connection-ready-cv conn)
                                    (http2-connection-ready-lock conn)
                                    :timeout 5)))

      ;; Allocate stream ID
      (setf (grpc-stream-stream-id stream) stream-id)
      (incf (http2-connection-next-stream-id conn) 2)

      ;; Register stream in connection
      (setf (gethash stream-id (http2-connection-active-calls conn)) stream)

      ;; Build and send HEADERS frame (no END_STREAM for streaming)
      (let ((headers (encode-grpc-request-headers
                      (grpc-stream-service stream)
                      (grpc-stream-method stream)
                      :authority (grpc-stream-authority stream)
                      :timeout (grpc-stream-timeout stream)
                      :metadata (grpc-stream-metadata stream))))

        (let ((headers-bytes (hpack-encode-headers hpack-ctx headers)))
          (let ((headers-frame (make-http2-frame
                                :length (length headers-bytes)
                                :type +frame-type-headers+
                                :flags +flag-end-headers+
                                :stream-id stream-id
                                :payload headers-bytes)))
            (write-frame-to-stream headers-frame (http2-connection-socket conn)))))

      (setf (grpc-stream-headers-sent stream) t)
      stream-id)))

(defun stream-send (stream message-bytes)
  "Send a message on the stream.

   Args:
     stream: grpc-stream
     message-bytes: Serialized protobuf message

   This does NOT close the send side - use stream-close-send for that."
  (bordeaux-threads:with-lock-held ((grpc-stream-send-lock stream))
    (when (grpc-stream-send-closed stream)
      (error "Stream send side already closed"))

    (unless (grpc-stream-headers-sent stream)
      (stream-start stream))

    ;; Send DATA frame without END_STREAM
    (let* ((conn (grpc-stream-connection stream))
           (stream-id (grpc-stream-stream-id stream))
           (grpc-message (encode-grpc-message message-bytes)))

      (let ((data-frame (make-http2-frame
                         :length (length grpc-message)
                         :type +frame-type-data+
                         :flags 0  ; No END_STREAM
                         :stream-id stream-id
                         :payload grpc-message)))
        (write-frame-to-stream data-frame (http2-connection-socket conn))))))

(defun stream-close-send (stream)
  "Close the send side of the stream (send END_STREAM).

   After this, no more messages can be sent."
  (bordeaux-threads:with-lock-held ((grpc-stream-send-lock stream))
    (when (grpc-stream-send-closed stream)
      (return-from stream-close-send nil))

    (unless (grpc-stream-headers-sent stream)
      (stream-start stream))

    ;; Send empty DATA frame with END_STREAM
    (let* ((conn (grpc-stream-connection stream))
           (stream-id (grpc-stream-stream-id stream)))

      (let ((data-frame (make-http2-frame
                         :length 0
                         :type +frame-type-data+
                         :flags +flag-end-stream+
                         :stream-id stream-id
                         :payload (make-byte-array 0))))
        (write-frame-to-stream data-frame (http2-connection-socket conn))))

    (setf (grpc-stream-send-closed stream) t)))

(defun stream-recv (stream &key timeout-ms)
  "Receive next message from stream.

   Returns:
     Message bytes, or NIL if stream is closed

   Blocks until a message is available or stream ends."
  (let ((start-time (get-internal-real-time))
        (timeout-internal (when timeout-ms
                           (* timeout-ms (/ internal-time-units-per-second 1000)))))

    (bordeaux-threads:with-lock-held ((grpc-stream-lock stream))
      (loop
        ;; Check for errors first
        (when (grpc-stream-error stream)
          (error (grpc-stream-error stream)))

        ;; Check if we have a message
        (unless (queue-empty-p (grpc-stream-response-queue stream))
          (return (queue-pop (grpc-stream-response-queue stream))))

        ;; Check if stream is closed (no more messages coming)
        (when (grpc-stream-recv-closed stream)
          (return nil))

        ;; Check timeout
        (when timeout-internal
          (let ((elapsed (- (get-internal-real-time) start-time)))
            (when (>= elapsed timeout-internal)
              (signal-grpc-deadline-exceeded "Stream receive timeout"))))

        ;; Wait for message or close
        (let ((remaining-time (when timeout-internal
                               (- timeout-internal
                                  (- (get-internal-real-time) start-time)))))
          (bordeaux-threads:condition-wait
           (grpc-stream-recv-cv stream)
           (grpc-stream-lock stream)
           :timeout (when remaining-time
                     (/ remaining-time internal-time-units-per-second))))))))

;;; Frame Handlers (called by connection's frame dispatcher)

(defmethod call-handle-headers ((stream grpc-stream) headers end-stream)
  "Handle HEADERS frame for streaming call."
  (bordeaux-threads:with-lock-held ((grpc-stream-lock stream))
    (if (grpc-stream-response-headers stream)
        ;; Trailers
        (progn
          (setf (grpc-stream-response-trailers stream) headers)

          ;; Extract status
          (let ((status-header (find "grpc-status" headers :key #'car :test #'string=)))
            (when status-header
              (setf (grpc-stream-status stream)
                    (parse-integer (cdr status-header) :junk-allowed t))))

          (let ((message-header (find "grpc-message" headers :key #'car :test #'string=)))
            (when message-header
              (setf (grpc-stream-status-message stream) (cdr message-header))))

          (when end-stream
            (setf (grpc-stream-recv-closed stream) t)
            (bordeaux-threads:condition-notify (grpc-stream-recv-cv stream))))

        ;; Response headers
        (progn
          (setf (grpc-stream-response-headers stream) headers)

          ;; Check for trailers-only response
          (let ((status-header (find "grpc-status" headers :key #'car :test #'string=)))
            (when status-header
              (setf (grpc-stream-status stream)
                    (parse-integer (cdr status-header) :junk-allowed t))
              (setf (grpc-stream-recv-closed stream) t)
              (bordeaux-threads:condition-notify (grpc-stream-recv-cv stream))))

          (when (and end-stream (not (grpc-stream-status stream)))
            ;; END_STREAM without grpc-status
            (setf (grpc-stream-error stream)
                  (make-grpc-error :code +grpc-status-unknown+
                                  :message "END_STREAM without grpc-status"))
            (setf (grpc-stream-recv-closed stream) t)
            (bordeaux-threads:condition-notify (grpc-stream-recv-cv stream)))))))

(defmethod call-handle-data ((stream grpc-stream) data end-stream)
  "Handle DATA frame for streaming call."
  (bordeaux-threads:with-lock-held ((grpc-stream-lock stream))
    ;; Decode gRPC message and add to queue
    (when (> (length data) 0)
      (multiple-value-bind (message-bytes compressed total-read)
          (decode-grpc-message data)
        (declare (ignore compressed total-read))
        (queue-push (grpc-stream-response-queue stream) message-bytes)))

    ;; Notify waiting receiver
    (bordeaux-threads:condition-notify (grpc-stream-recv-cv stream))

    ;; If END_STREAM but no trailers yet, wait for trailers
    ;; (They should arrive in a HEADERS frame)
    (when end-stream
      ;; Mark that we won't receive more DATA
      ;; But wait for HEADERS/trailers for final status
      nil)))

(defmethod call-handle-rst-stream ((stream grpc-stream) error-code)
  "Handle RST_STREAM frame for streaming call."
  (bordeaux-threads:with-lock-held ((grpc-stream-lock stream))
    (setf (grpc-stream-error stream)
          (make-condition 'grpc-error
                         :code +grpc-status-internal+
                         :message (format nil "Stream reset with error code ~D" error-code)))
    (setf (grpc-stream-recv-closed stream) t)
    (bordeaux-threads:condition-notify (grpc-stream-recv-cv stream))))
