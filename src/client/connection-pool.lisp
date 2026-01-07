;;;; connection-pool.lisp - Connection pooling for gRPC clients
;;;
;;; Simple connection pool implementation for managing HTTP/2 connections

(in-package #:clgrpc.client)

;;; Connection Pool

(defstruct connection-pool
  "Pool of HTTP/2 connections to a target"
  (target "" :type string)
  (secure t :type boolean)
  (connections (make-array 0 :adjustable t :fill-pointer 0))
  (max-connections 10 :type fixnum)
  (lock (bordeaux-threads:make-lock "connection-pool-lock")))

(defun make-grpc-connection-pool (target &key (secure t) (max-connections 10))
  "Create a connection pool for a gRPC target.

   Args:
     target: Host:port string (e.g., \"localhost:50051\")
     secure: Use TLS if true (default: t)
     max-connections: Maximum number of pooled connections (default: 10)

   Returns:
     connection-pool structure"
  (make-connection-pool
   :target target
   :secure secure
   :max-connections max-connections))

(defun pool-get-connection (pool)
  "Get a connection from the pool (reuse existing or create new).

   Args:
     pool: connection-pool structure

   Returns:
     http2-connection"
  (bordeaux-threads:with-lock-held ((connection-pool-lock pool))
    ;; Try to reuse existing connection
    (let ((existing (find-if (lambda (conn)
                              (and (http2-connection-open-p conn)
                                   (not (http2-connection-goaway-received conn))))
                            (connection-pool-connections pool))))
      (when existing
        (return-from pool-get-connection existing)))

    ;; Create new connection if under limit
    (when (< (length (connection-pool-connections pool))
             (connection-pool-max-connections pool))
      (let ((new-conn (create-http2-connection
                       (connection-pool-target pool)
                       :secure (connection-pool-secure pool))))
        (vector-push-extend new-conn (connection-pool-connections pool))
        (return-from pool-get-connection new-conn)))

    ;; Wait for connection to become available (for now, just return first)
    ;; TODO: Implement proper waiting/queueing
    (aref (connection-pool-connections pool) 0)))

(defun pool-return-connection (pool connection)
  "Return a connection to the pool (no-op for now, connection stays in pool).

   Args:
     pool: connection-pool structure
     connection: http2-connection"
  (declare (ignore pool connection))
  ;; Connections stay in pool for reuse
  nil)

(defun pool-close-all (pool)
  "Close all connections in the pool.

   Args:
     pool: connection-pool structure"
  (bordeaux-threads:with-lock-held ((connection-pool-lock pool))
    (loop for conn across (connection-pool-connections pool)
          do (when (http2-connection-open-p conn)
               (http2-connection-close conn)))
    (setf (fill-pointer (connection-pool-connections pool)) 0)))

;;; HTTP/2 Connection Creation

(defun create-http2-connection (target &key (secure t))
  "Create and establish an HTTP/2 connection to target.

   Args:
     target: Host:port string (e.g., \"localhost:50051\")
     secure: Use TLS if true

   Returns:
     http2-connection (connected and ready)

   Signals:
     error if connection fails"
  ;; Parse target
  (let* ((colon-pos (position #\: target))
         (host (if colon-pos
                  (subseq target 0 colon-pos)
                  target))
         (port (if colon-pos
                  (parse-integer (subseq target (1+ colon-pos)) :junk-allowed t)
                  (if secure 443 80))))

    ;; Create TCP socket
    (let ((socket (usocket:socket-connect host port
                                          :element-type '(unsigned-byte 8))))

      ;; Upgrade to TLS if secure
      (when secure
        (let ((tls-stream (tls-wrap-socket socket host)))
          (setf socket tls-stream)))

      ;; Create HTTP/2 connection
      (let ((conn (make-http2-connection
                   :socket socket
                   :role :client
                   :encoder-context (make-hpack-context)
                   :decoder-context (make-hpack-context))))

        ;; Send client connection preface
        (http2-send-client-preface conn)

        ;; Send initial SETTINGS frame
        (http2-send-settings conn nil)  ; Empty settings for now

        ;; Start frame reader thread
        (start-connection-frame-reader conn)

        conn))))

(defun http2-send-client-preface (connection)
  "Send HTTP/2 client connection preface.

   Preface is: \"PRI * HTTP/2.0\\r\\n\\r\\nSM\\r\\n\\r\\n\" (24 bytes)"
  (let ((preface (http2-client-preface-bytes))
        (socket (http2-connection-socket connection)))
    (write-sequence preface socket)
    (force-output socket)))

(defun http2-send-settings (connection settings)
  "Send SETTINGS frame.

   Args:
     connection: http2-connection
     settings: List of (parameter . value) pairs (nil for empty)"
  (let ((payload (if settings
                    (encode-settings-frame settings)
                    (make-byte-array 0))))
    (let ((frame (make-http2-frame
                  :length (length payload)
                  :type +frame-type-settings+
                  :flags 0
                  :stream-id 0
                  :payload payload)))
      (http2-write-frame (http2-connection-socket connection) frame))))

(defun start-connection-frame-reader (connection)
  "Start background thread to read frames from connection.

   Thread reads frames in loop and dispatches to appropriate handlers."
  (bordeaux-threads:make-thread
   (lambda ()
     (handler-case
         (loop
           (let ((frame (http2-read-frame (http2-connection-socket connection))))
             (unless frame
               ;; EOF - connection closed
               (return))

             ;; Dispatch frame to handler
             (dispatch-frame-to-call connection frame)))
       (error (e)
         ;; Log error and close connection
         (format *error-output* "Frame reader error: ~A~%" e)
         (http2-connection-close connection))))
   :name "http2-frame-reader"))

(defun dispatch-frame-to-call (connection frame)
  "Dispatch received frame to the appropriate call handler.

   Args:
     connection: http2-connection
     frame: http2-frame"
  ;; For now, simplified: find call by stream-id
  ;; TODO: Implement proper call registry in connection
  (let ((stream-id (http2-frame-stream-id frame))
        (frame-type (http2-frame-type frame)))

    (cond
      ((= frame-type +frame-type-headers+)
       ;; Decode headers
       (let* ((decoder-ctx (http2-connection-decoder-context connection))
              (headers (hpack-decode-headers decoder-ctx (http2-frame-payload frame)))
              (end-stream (logtest (http2-frame-flags frame) +flag-end-stream+))
              (call (find-call-by-stream-id connection stream-id)))
         (when call
           (call-handle-headers call headers end-stream))))

      ((= frame-type +frame-type-data+)
       (let ((end-stream (logtest (http2-frame-flags frame) +flag-end-stream+))
             (call (find-call-by-stream-id connection stream-id)))
         (when call
           (call-handle-data call (http2-frame-payload frame) end-stream))))

      ((= frame-type +frame-type-rst-stream+)
       (let ((error-code (decode-uint32-be (http2-frame-payload frame) 0))
             (call (find-call-by-stream-id connection stream-id)))
         (when call
           (call-handle-rst-stream call error-code))))

      ((= frame-type +frame-type-settings+)
       ;; Handle SETTINGS (ACK or update)
       (when (zerop (logand (http2-frame-flags frame) +flag-ack+))
         ;; Not an ACK - send ACK back
         (let ((ack-frame (make-http2-frame
                           :length 0
                           :type +frame-type-settings+
                           :flags +flag-ack+
                           :stream-id 0
                           :payload (make-byte-array 0))))
           (http2-write-frame (http2-connection-socket connection) ack-frame))))

      ((= frame-type +frame-type-goaway+)
       ;; Server is shutting down connection
       (setf (http2-connection-goaway-received connection) t)))))

(defun find-call-by-stream-id (connection stream-id)
  "Find active call by stream ID.

   TODO: Implement proper call registry.
   For now, return nil (calls will be tracked in higher layer)."
  (declare (ignore connection stream-id))
  nil)
