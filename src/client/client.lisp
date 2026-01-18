;;;; client.lisp - High-level gRPC client API
;;;
;;; User-facing API for making gRPC calls

(in-package #:clgrpc.client)

;;; Channel - represents a connection to a gRPC server

(defstruct grpc-channel
  "gRPC channel (connection to server)"
  (target "" :type string)
  (pool nil :type (or null connection-pool))
  (active-calls (make-hash-table) :type hash-table)  ; stream-id -> grpc-call
  (calls-lock (bordeaux-threads:make-lock "channel-calls-lock"))
  (closed nil :type boolean))

(defun make-channel (target &key (secure t))
  "Create a gRPC channel to the specified target.

   Args:
     target: Host:port string (e.g., \"localhost:50051\")
     secure: Use TLS if true (default: true for production)

   Returns:
     grpc-channel

   Example:
     (make-channel \"localhost:50051\" :secure nil)"
  (make-grpc-channel
   :target target
   :pool (make-grpc-connection-pool target :secure secure)))

(defun close-channel (channel)
  "Close a gRPC channel and all its connections.

   Args:
     channel: grpc-channel

   Side effects:
     Closes all pooled connections
     Marks channel as closed"
  (bordeaux-threads:with-lock-held ((grpc-channel-calls-lock channel))
    (setf (grpc-channel-closed channel) t)
    (pool-close-all (grpc-channel-pool channel))
    (clrhash (grpc-channel-active-calls channel))))

;;; Unary RPC

(defun call-unary (channel service method request &key timeout metadata)
  "Make a unary gRPC call (single request, single response).

   Args:
     channel: grpc-channel created with make-channel
     service: Service name string (e.g., \"helloworld.Greeter\")
     method: Method name string (e.g., \"SayHello\")
     request: Request message bytes (serialized protobuf)
     timeout: Optional timeout in milliseconds
     metadata: Optional list of (name . value) pairs for custom headers

   Returns:
     Response message bytes (serialized protobuf, NOT gRPC-framed)

   Signals:
     grpc-error if call fails
     error if channel is closed

   Example:
     (let ((channel (make-channel \"localhost:50051\" :secure nil)))
       (unwind-protect
            (let ((response (call-unary channel
                                       \"helloworld.Greeter\"
                                       \"SayHello\"
                                       request-bytes
                                       :timeout 5000)))
              (process-response response))
         (close-channel channel)))"
  (when (grpc-channel-closed channel)
    (error "Channel is closed"))

  ;; Get connection from pool
  (let ((connection (pool-get-connection (grpc-channel-pool channel))))

    ;; Create call
    (let ((call (make-grpc-call
                 :connection connection
                 :service service
                 :method method
                 :authority (grpc-channel-target channel)
                 :timeout timeout
                 :metadata metadata)))

      (unwind-protect
           (progn
             ;; Execute call (send request, wait for response)
             (multiple-value-bind (response-data status status-message)
                 (execute-unary-call call request)

               ;; Decode gRPC message framing
               (multiple-value-bind (message-bytes compressed total-read)
                   (decode-grpc-message response-data)
                 (declare (ignore compressed total-read))
                 ;; Return response bytes, status, and status message
                 (values message-bytes status status-message))))

        ;; Cleanup: unregister call from connection
        (when (grpc-call-stream-id call)
          (bt:with-lock-held ((http2-connection-active-calls-lock connection))
            (remhash (grpc-call-stream-id call)
                    (http2-connection-active-calls connection))))

        ;; Return connection to pool
        (pool-return-connection (grpc-channel-pool channel) connection)))))

;;; Call Registration (for frame dispatch)

(defun channel-register-call (channel call stream-id)
  "Register active call in channel for frame dispatching.

   Args:
     channel: grpc-channel
     call: grpc-call
     stream-id: HTTP/2 stream ID"
  (bordeaux-threads:with-lock-held ((grpc-channel-calls-lock channel))
    (setf (gethash stream-id (grpc-channel-active-calls channel)) call)))

(defun channel-unregister-call (channel stream-id)
  "Unregister call from channel.

   Args:
     channel: grpc-channel
     stream-id: HTTP/2 stream ID"
  (bordeaux-threads:with-lock-held ((grpc-channel-calls-lock channel))
    (remhash stream-id (grpc-channel-active-calls channel))))

(defun channel-find-call (channel stream-id)
  "Find active call by stream ID.

   Args:
     channel: grpc-channel
     stream-id: HTTP/2 stream ID

   Returns:
     grpc-call or nil"
  (bordeaux-threads:with-lock-held ((grpc-channel-calls-lock channel))
    (gethash stream-id (grpc-channel-active-calls channel))))

;;; Streaming RPCs

(defun call-client-streaming (channel service method &key timeout metadata)
  "Start a client streaming call (client sends many → server sends one).

   Args:
     channel: grpc-channel created with make-channel
     service: Service name string (e.g., \"helloworld.Greeter\")
     method: Method name string (e.g., \"StreamingHello\")
     timeout: Optional timeout in milliseconds
     metadata: Optional list of (name . value) pairs for custom headers

   Returns:
     grpc-stream - use stream-send to send messages, stream-close-send when done,
                   then stream-recv to get the single response

   Example:
     (let* ((channel (make-channel \"localhost:50051\" :secure nil))
            (stream (call-client-streaming channel \"service\" \"method\")))
       (unwind-protect
            (progn
              (stream-send stream request1-bytes)
              (stream-send stream request2-bytes)
              (stream-close-send stream)
              (let ((response (stream-recv stream)))
                (process-response response)))
         (close-channel channel)))"
  (when (grpc-channel-closed channel)
    (error "Channel is closed"))

  ;; Get connection from pool
  (let ((connection (pool-get-connection (grpc-channel-pool channel))))

    ;; Create stream
    (let ((stream (create-stream connection service method
                                :authority (grpc-channel-target channel)
                                :timeout timeout
                                :metadata metadata)))

      ;; Start stream (sends HEADERS)
      (stream-start stream)

      stream)))

(defun call-server-streaming (channel service method request &key timeout metadata)
  "Start a server streaming call (client sends one → server sends many).

   Args:
     channel: grpc-channel created with make-channel
     service: Service name string (e.g., \"helloworld.Greeter\")
     method: Method name string (e.g., \"StreamingHello\")
     request: Request message bytes (serialized protobuf)
     timeout: Optional timeout in milliseconds
     metadata: Optional list of (name . value) pairs for custom headers

   Returns:
     grpc-stream - use stream-recv repeatedly to receive responses (returns nil when done)

   Example:
     (let* ((channel (make-channel \"localhost:50051\" :secure nil))
            (stream (call-server-streaming channel \"service\" \"method\" request-bytes)))
       (unwind-protect
            (loop for response = (stream-recv stream)
                  while response
                  do (process-response response))
         (close-channel channel)))"
  (when (grpc-channel-closed channel)
    (error "Channel is closed"))

  ;; Get connection from pool
  (let ((connection (pool-get-connection (grpc-channel-pool channel))))

    ;; Create stream
    (let ((stream (create-stream connection service method
                                :authority (grpc-channel-target channel)
                                :timeout timeout
                                :metadata metadata)))

      ;; Send request and close send side
      (stream-send stream request)
      (stream-close-send stream)

      stream)))

(defun call-bidirectional-streaming (channel service method &key timeout metadata)
  "Start a bidirectional streaming call (both send many).

   Args:
     channel: grpc-channel created with make-channel
     service: Service name string (e.g., \"helloworld.Greeter\")
     method: Method name string (e.g., \"StreamingHello\")
     timeout: Optional timeout in milliseconds
     metadata: Optional list of (name . value) pairs for custom headers

   Returns:
     grpc-stream - use stream-send to send messages, stream-recv to receive messages
                   call stream-close-send when done sending

   Example:
     (let* ((channel (make-channel \"localhost:50051\" :secure nil))
            (stream (call-bidirectional-streaming channel \"service\" \"method\")))
       (unwind-protect
            (progn
              ;; Can send and receive in any order
              (stream-send stream request1-bytes)
              (let ((response1 (stream-recv stream)))
                (process-response response1))
              (stream-send stream request2-bytes)
              (let ((response2 (stream-recv stream)))
                (process-response response2))
              (stream-close-send stream))
         (close-channel channel)))"
  (when (grpc-channel-closed channel)
    (error "Channel is closed"))

  ;; Get connection from pool
  (let ((connection (pool-get-connection (grpc-channel-pool channel))))

    ;; Create stream
    (let ((stream (create-stream connection service method
                                :authority (grpc-channel-target channel)
                                :timeout timeout
                                :metadata metadata)))

      ;; Start stream (sends HEADERS)
      (stream-start stream)

      stream)))

;;; Helper: Connect call registry to connection pool
;;; Note: find-call-by-stream-id is defined in connection-pool.lisp
