;;;; server.lisp - gRPC server implementation
;;;
;;; Main server lifecycle and request handling

(in-package #:clgrpc.server)

;;; Server Structure

(defstruct grpc-server
  "gRPC server instance"
  (port 50051 :type fixnum)
  (router nil :type (or null grpc-router))
  (listener-socket nil)
  (running nil :type boolean)
  (connections (make-array 0 :adjustable t :fill-pointer 0))
  (lock (bordeaux-threads:make-lock "server-lock"))
  (listener-thread nil))

;;; Server Lifecycle

(defun make-server (&key (port 50051))
  "Create a new gRPC server.

   Args:
     port: Port to listen on (default: 50051)

   Returns:
     grpc-server"
  (make-grpc-server
   :port port
   :router (make-router)))

(defun start-server (server)
  "Start the gRPC server (begin listening for connections).

   Args:
     server: grpc-server

   Side effects:
     Starts listener thread
     Binds to port"
  (bordeaux-threads:with-lock-held ((grpc-server-lock server))
    (when (grpc-server-running server)
      (error "Server is already running"))

    ;; Create TCP listener socket using transport layer
    (let ((socket (clgrpc.transport:make-tcp-server (grpc-server-port server)
                                                    :host "0.0.0.0"
                                                    :reuse-address t)))
      (setf (grpc-server-listener-socket server) socket)
      (setf (grpc-server-running server) t)

      ;; Start listener thread
      (setf (grpc-server-listener-thread server)
            (bordeaux-threads:make-thread
             (lambda () (server-listen-loop server))
             :name (format nil "grpc-server-~D" (grpc-server-port server))))))

  (format t "gRPC server listening on port ~D~%" (grpc-server-port server)))

(defun stop-server (server)
  "Stop the gRPC server.

   Args:
     server: grpc-server

   Side effects:
     Closes all connections
     Stops listener thread"
  (bordeaux-threads:with-lock-held ((grpc-server-lock server))
    (unless (grpc-server-running server)
      (return-from stop-server nil))

    (setf (grpc-server-running server) nil)

    ;; Close all client connections
    (loop for conn across (grpc-server-connections server)
          do (ignore-errors (connection-close conn)))
    (setf (fill-pointer (grpc-server-connections server)) 0)

    ;; Close listener socket
    (when (grpc-server-listener-socket server)
      (ignore-errors (usocket:socket-close (grpc-server-listener-socket server)))
      (setf (grpc-server-listener-socket server) nil)))

  (format t "gRPC server stopped~%"))

;;; Connection Handling

(defun server-listen-loop (server)
  "Main server loop - accepts incoming connections.

   Runs in dedicated thread."
  (handler-case
      (loop while (grpc-server-running server)
            do (let ((client-socket (clgrpc.transport:accept-connection
                                    (grpc-server-listener-socket server))))
                 (when client-socket
                   (server-handle-connection server client-socket))))
    (error (e)
      (format *error-output* "Server listen loop error: ~A~%" e)
      (bordeaux-threads:with-lock-held ((grpc-server-lock server))
        (setf (grpc-server-running server) nil)))))

(defun server-handle-connection (server client-socket)
  "Handle a new client connection.

   Creates HTTP/2 connection and spawns handler thread.

   Args:
     server: grpc-server
     client-socket: tcp-socket from transport layer"
  ;; Wrap socket with buffering for efficient HTTP/2 frame I/O
  (let ((buffered-socket (clgrpc.transport:wrap-socket-with-buffer client-socket)))

    ;; Create HTTP/2 connection
    (let ((conn (make-http2-connection
                 :socket buffered-socket
                 :is-client nil
                 :hpack-encoder (make-hpack-context)
                 :hpack-decoder (make-hpack-context))))

      ;; Add to server's connection list
      (bordeaux-threads:with-lock-held ((grpc-server-lock server))
        (vector-push-extend conn (grpc-server-connections server)))

      ;; Spawn connection handler thread
      (bordeaux-threads:make-thread
       (lambda () (server-connection-loop server conn))
       :name "grpc-connection-handler"))))

(defun server-connection-loop (server connection)
  "Handle frames for a single connection.

   Runs in dedicated thread per connection.

   Args:
     server: grpc-server
     connection: http2-connection"
  (handler-case
      (progn
        ;; Read and validate client preface
        (server-read-client-preface connection)

        ;; Send server SETTINGS
        (server-send-settings connection)

        ;; Process frames
        (loop
          (let ((frame (read-frame-from-stream (http2-connection-socket connection))))
            (unless frame
              ;; EOF - client disconnected
              (return))

            ;; Dispatch frame
            (server-dispatch-frame server connection frame))))
    (error (e)
      (format *error-output* "Connection error: ~A~%" e)))

  ;; Cleanup
  (ignore-errors (connection-close connection))
  (bordeaux-threads:with-lock-held ((grpc-server-lock server))
    (setf (grpc-server-connections server)
          (delete connection (grpc-server-connections server)))))

(defun server-read-client-preface (connection)
  "Read and validate HTTP/2 client preface.

   Preface is 24 bytes: \"PRI * HTTP/2.0\\r\\n\\r\\nSM\\r\\n\\r\\n\""
  (let ((buffered-socket (http2-connection-socket connection)))
    (let ((preface (clgrpc.transport:buffered-read-bytes buffered-socket 24)))
      (unless (equalp preface (http2-client-preface-bytes))
        (error "Invalid client preface")))))

(defun server-send-settings (connection)
  "Send initial SETTINGS frame to client."
  (let ((frame (make-http2-frame
                :length 0
                :type +frame-type-settings+
                :flags 0
                :stream-id 0
                :payload (make-byte-array 0))))
    (write-frame-to-stream frame (http2-connection-socket connection))))

;;; Frame Dispatching

(defun server-dispatch-frame (server connection frame)
  "Dispatch received frame to appropriate handler.

   Args:
     server: grpc-server
     connection: http2-connection
     frame: http2-frame"
  (let ((frame-type (frame-type frame))
        (stream-id (frame-stream-id frame)))

    (cond
      ((= frame-type +frame-type-headers+)
       (server-handle-headers-frame server connection frame))

      ((= frame-type +frame-type-data+)
       (server-handle-data-frame server connection frame))

      ((= frame-type +frame-type-settings+)
       (server-handle-settings-frame connection frame))

      ((= frame-type +frame-type-ping+)
       (server-handle-ping-frame connection frame))

      ((= frame-type +frame-type-window-update+)
       ;; TODO: Handle window updates
       nil)

      (t
       ;; Unknown frame type - ignore
       nil))))

(defun server-handle-headers-frame (server connection frame)
  "Handle HEADERS frame - start of new RPC request.

   Args:
     server: grpc-server
     connection: http2-connection
     frame: http2-frame with HEADERS"
  (let* ((stream-id (frame-stream-id frame))
         (decoder-ctx (http2-connection-hpack-decoder connection))
         (headers (hpack-decode-headers decoder-ctx (frame-payload frame)))
         (path (cdr (assoc ":path" headers :test #'string=))))

    ;; Route request to handler
    (multiple-value-bind (handler service method)
        (route-request (grpc-server-router server) path)

      ;; Create handler context
      (let ((context (make-handler-context
                      :stream-id stream-id
                      :connection connection
                      :metadata headers)))

        ;; Spawn thread to handle request (allows concurrent requests)
        (bordeaux-threads:make-thread
         (lambda () (server-handle-request server connection stream-id
                                          handler service method context))
         :name "grpc-request-handler")))))

(defun server-handle-data-frame (server connection frame)
  "Handle DATA frame - request body.

   For now, accumulate in connection state.
   TODO: Implement proper stream-based request accumulation."
  (declare (ignore server connection frame))
  ;; TODO: Accumulate request data per stream
  nil)

(defun server-handle-settings-frame (connection frame)
  "Handle SETTINGS frame.

   Args:
     connection: http2-connection
     frame: http2-frame with SETTINGS"
  (let ((flags (frame-flags frame)))
    (if (logtest flags +flag-ack+)
        ;; ACK - nothing to do
        nil
        ;; New settings - send ACK
        (let ((ack-frame (make-http2-frame
                          :length 0
                          :type +frame-type-settings+
                          :flags +flag-ack+
                          :stream-id 0
                          :payload (make-byte-array 0))))
          (write-frame-to-stream ack-frame (http2-connection-socket connection))))))

(defun server-handle-ping-frame (connection frame)
  "Handle PING frame - send PONG.

   Args:
     connection: http2-connection
     frame: http2-frame with PING"
  (let ((pong-frame (make-http2-frame
                     :length (frame-length frame)
                     :type +frame-type-ping+
                     :flags +flag-ack+
                     :stream-id 0
                     :payload (frame-payload frame))))
    (write-frame-to-stream pong-frame (http2-connection-socket connection))))

;;; Request Handling

(defun server-handle-request (server connection stream-id handler service method context)
  "Handle a complete gRPC request.

   Args:
     server: grpc-server
     connection: http2-connection
     stream-id: HTTP/2 stream ID
     handler: Handler instance
     service: Service name
     method: Method name
     context: handler-context"
  (declare (ignore server))

  ;; For now, assume request data is empty (will fix with proper DATA frame handling)
  (let ((request-bytes (make-byte-array 0)))

    (handler-case
        (progn
          ;; Call handler
          (multiple-value-bind (response-bytes status-code status-message response-metadata)
              (handle-unary handler service method request-bytes context)

            ;; Send response
            (server-send-response connection stream-id
                                 response-bytes status-code status-message
                                 response-metadata)))
      (error (e)
        ;; Error in handler - send INTERNAL error
        (server-send-response connection stream-id
                             nil +grpc-status-internal+
                             (format nil "Internal error: ~A" e)
                             nil)))))

(defun server-send-response (connection stream-id response-bytes status-code status-message response-metadata)
  "Send gRPC response (headers + data + trailers).

   Args:
     connection: http2-connection
     stream-id: HTTP/2 stream ID
     response-bytes: Serialized protobuf response (or nil)
     status-code: gRPC status code
     status-message: Status message (or nil)
     response-metadata: Response metadata (or nil)"
  (let ((encoder-ctx (http2-connection-hpack-encoder connection)))

    ;; Send response headers
    (let ((headers (encode-grpc-response-headers :metadata response-metadata)))
      (let ((headers-bytes (hpack-encode-headers encoder-ctx headers)))
        (let ((headers-frame (make-http2-frame
                              :length (length headers-bytes)
                              :type +frame-type-headers+
                              :flags +flag-end-headers+
                              :stream-id stream-id
                              :payload headers-bytes)))
          (write-frame-to-stream headers-frame (http2-connection-socket connection)))))

    ;; Send DATA frame if we have response
    (when response-bytes
      (let ((grpc-message (encode-grpc-message response-bytes)))
        (let ((data-frame (make-http2-frame
                           :length (length grpc-message)
                           :type +frame-type-data+
                           :flags 0
                           :stream-id stream-id
                           :payload grpc-message)))
          (write-frame-to-stream data-frame (http2-connection-socket connection)))))

    ;; Send trailers with status
    (let ((trailers (encode-grpc-trailers status-code status-message)))
      (let ((trailers-bytes (hpack-encode-headers encoder-ctx trailers)))
        (let ((trailers-frame (make-http2-frame
                               :length (length trailers-bytes)
                               :type +frame-type-headers+
                               :flags (logior +flag-end-headers+ +flag-end-stream+)
                               :stream-id stream-id
                               :payload trailers-bytes)))
          (write-frame-to-stream trailers-frame (http2-connection-socket connection)))))))
