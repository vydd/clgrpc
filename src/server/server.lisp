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

;;; Per-stream request state
(defstruct server-stream-state
  "Tracks request state for a single stream"
  (stream-id nil :type fixnum)
  (headers nil :type list)
  (data nil :type (or null (vector (unsigned-byte 8))))
  (complete nil :type boolean)
  (lock (bordeaux-threads:make-lock "stream-state-lock"))
  ;; For server-streaming RPCs, we store handler info to invoke after receiving request
  (rpc-type nil :type (or null keyword))
  (handler nil)
  (service nil :type (or null string))
  (method nil :type (or null string)))

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
      (debug-log "Server listen loop error: ~A~%" e)
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

      ;; active-calls hash table will store server-stream-state for server connections
      ;; (already initialized in make-http2-connection)

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
      (debug-log "Connection error: ~A~%" e)))

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
         (end-stream (logtest (frame-flags frame) +flag-end-stream+)))

    (debug-log "SERVER: HEADERS stream=~D end-stream=~A~%" stream-id end-stream)

    ;; Extract path to determine RPC type
    (let ((path (or (cdr (assoc :path headers))
                    (cdr (assoc ":path" headers :test #'string=)))))

      (multiple-value-bind (handler service method rpc-type)
          (route-request (grpc-server-router server) path)

        (debug-log "SERVER: RPC type=~A service=~A method=~A~%"
                rpc-type service method)

        (cond
          ;; Client-streaming or Bidirectional - invoke handler immediately
          ((member rpc-type '(:client-streaming :bidirectional))
           (let ((stream (make-grpc-server-stream
                          :connection connection
                          :stream-id stream-id
                          :headers headers
                          :recv-closed end-stream
                          :send-closed nil
                          :request-queue (make-queue))))

             ;; Register stream
             (setf (gethash stream-id (http2-connection-active-calls connection))
                   stream)

             ;; Send initial response headers (for bidirectional)
             (when (eq rpc-type :bidirectional)
               (server-send-initial-headers connection stream-id))

             ;; Spawn handler thread immediately
             (let ((context (make-handler-context
                             :stream-id stream-id
                             :connection connection
                             :metadata headers)))

               (bordeaux-threads:make-thread
                (lambda () (server-handle-streaming-request
                            handler service method stream context rpc-type))
                :name (format nil "grpc-streaming-~D" stream-id)))))

          ;; Server-streaming - need to wait for request, use state accumulation
          ((eq rpc-type :server-streaming)
           (let ((stream-state (make-server-stream-state
                                :stream-id stream-id
                                :headers headers
                                :data nil
                                :complete end-stream
                                :rpc-type :server-streaming  ; Mark as server-streaming
                                :handler handler
                                :service service
                                :method method)))

             ;; Register stream state
             (setf (gethash stream-id (http2-connection-active-calls connection))
                   stream-state)

             ;; If END_STREAM, request is complete (shouldn't happen for normal calls)
             (when end-stream
               (server-process-complete-request server connection stream-id stream-state))))

          ;; Unary RPC - use existing accumulation approach
          (t
           (let ((stream-state (make-server-stream-state
                                :stream-id stream-id
                                :headers headers
                                :data nil
                                :complete end-stream)))

             ;; Register stream state
             (setf (gethash stream-id (http2-connection-active-calls connection))
                   stream-state)

             ;; If END_STREAM, request is complete (headers-only request)
             (when end-stream
               (server-process-complete-request server connection stream-id stream-state))))))))

(defun server-handle-data-frame (server connection frame)
  "Handle DATA frame - request body.

   For unary RPCs: accumulates data until END_STREAM, then processes.
   For streaming RPCs: decodes gRPC messages and adds to queue."
  (let ((stream-id (frame-stream-id frame))
        (data (frame-payload frame))
        (end-stream (logtest (frame-flags frame) +flag-end-stream+)))

    (debug-log "SERVER: DATA stream=~D len=~D end-stream=~A~%"
            stream-id (length data) end-stream)

    ;; Find stream state (could be server-stream-state or grpc-server-stream)
    (let ((stream-state
            (gethash stream-id (http2-connection-active-calls connection))))

      (unless stream-state
        (debug-log "WARNING: DATA for unknown stream ~D~%" stream-id)
        (return-from server-handle-data-frame nil))

      (cond
        ;; Streaming RPC - decode messages and add to queue
        ((typep stream-state 'grpc-server-stream)
         (debug-log "SERVER: Streaming RPC - decoding gRPC messages~%")

         ;; Decode gRPC message from DATA frame
         (when (> (length data) 0)
           (handler-case
               (multiple-value-bind (message-bytes compressed total-read)
                   (decode-grpc-message data)
                 (declare (ignore compressed total-read))

                 (debug-log "SERVER: Decoded message (~D bytes), adding to queue~%"
                         (length message-bytes))

                 ;; Add to stream queue
                 (server-stream-add-message stream-state message-bytes))
             (error (e)
               (debug-log "SERVER: Error decoding message: ~A~%" e))))

         ;; Mark stream closed if END_STREAM
         (when end-stream
           (debug-log "SERVER: Received END_STREAM, marking recv closed~%")
           (server-stream-mark-recv-closed stream-state)))

        ;; Unary RPC - accumulate data
        (t
         (bt:with-lock-held ((server-stream-state-lock stream-state))
           (if (server-stream-state-data stream-state)
               ;; Append to existing data
               (let* ((existing (server-stream-state-data stream-state))
                      (new-len (+ (length existing) (length data)))
                      (result (make-byte-array new-len)))
                 (replace result existing)
                 (replace result data :start1 (length existing))
                 (setf (server-stream-state-data stream-state) result))
               ;; First data chunk
               (setf (server-stream-state-data stream-state) (copy-seq data)))

           ;; Mark complete if END_STREAM
           (when end-stream
             (setf (server-stream-state-complete stream-state) t)))

         ;; If request is complete, process it
         (when end-stream
           (server-process-complete-request server connection stream-id stream-state))))))))

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

(defun server-send-initial-headers (connection stream-id)
  "Send initial response headers for streaming RPC.

   Args:
     connection: http2-connection
     stream-id: HTTP/2 stream ID"
  ;; NOTE: Flush happens OUTSIDE lock to avoid blocking frame reader's ACKs
  (let ((socket (http2-connection-socket connection)))
    (bt:with-lock-held ((http2-connection-write-lock connection))
      (let ((encoder-ctx (http2-connection-hpack-encoder connection)))
        (let ((headers (encode-grpc-response-headers :metadata nil)))
          (let ((headers-bytes (hpack-encode-headers encoder-ctx headers)))
            (let ((headers-frame (make-http2-frame
                                  :length (length headers-bytes)
                                  :type +frame-type-headers+
                                  :flags +flag-end-headers+
                                  :stream-id stream-id
                                  :payload headers-bytes)))
              (write-frame-to-stream headers-frame socket :flush nil))))))
    ;; Flush outside the write lock
    (clgrpc.transport:buffered-flush socket)))

(defun server-handle-streaming-request (handler service method stream context rpc-type)
  "Handle client-streaming or bidirectional streaming gRPC request.

   Args:
     handler: Handler instance
     service: Service name
     method: Method name
     stream: grpc-server-stream
     context: handler-context
     rpc-type: RPC type (:client-streaming or :bidirectional)"
  (debug-log "SERVER: Streaming handler service=~A method=~A rpc-type=~A~%"
          service method rpc-type)

  (handler-case
      (progn
        ;; Dispatch to appropriate handler method
        (multiple-value-bind (status-code status-message response-metadata)
            (ecase rpc-type
              (:client-streaming
               (handle-client-streaming handler service method stream context))
              (:bidirectional
               (handle-bidirectional-streaming handler service method stream context)))

          (debug-log "SERVER: Streaming handler returned status=~D~%"
                  status-code)

          ;; Close stream with status
          (server-stream-close stream status-code status-message response-metadata)))

    (error (e)
      ;; Error in handler - send INTERNAL error
      (debug-log "SERVER: Streaming handler error: ~A~%" e)
      (server-stream-close stream +grpc-status-internal+
                          (format nil "Internal error: ~A" e)
                          nil))))

(defun server-handle-server-streaming-request (handler service method request-bytes stream context)
  "Handle server-streaming gRPC request.

   Args:
     handler: Handler instance
     service: Service name
     method: Method name
     request-bytes: Initial request message
     stream: grpc-server-stream
     context: handler-context"
  (debug-log "SERVER: Server-streaming handler service=~A method=~A request-len=~D~%"
          service method (length request-bytes))

  (handler-case
      (progn
        ;; Call handler with request-bytes
        (multiple-value-bind (status-code status-message response-metadata)
            (handle-server-streaming handler service method request-bytes stream context)

          (debug-log "SERVER: Server-streaming handler returned status=~D~%"
                  status-code)

          ;; Close stream with status
          (server-stream-close stream status-code status-message response-metadata)))

    (error (e)
      ;; Error in handler - send INTERNAL error
      (debug-log "SERVER: Server-streaming handler error: ~A~%" e)
      (server-stream-close stream +grpc-status-internal+
                          (format nil "Internal error: ~A" e)
                          nil))))

(defun server-process-complete-request (server connection stream-id stream-state)
  "Process a complete request (all DATA frames received).

   Args:
     server: grpc-server
     connection: http2-connection
     stream-id: Stream ID
     stream-state: server-stream-state with complete request"
  (let ((headers (server-stream-state-headers stream-state))
        (request-data (server-stream-state-data stream-state)))

    (debug-log "SERVER: Processing complete request stream=~D data-len=~D~%"
            stream-id (if request-data (length request-data) 0))
    (debug-log "SERVER: Headers: ~S~%" headers)

    ;; Extract gRPC message from HTTP/2 DATA payload
    (let ((request-bytes
            (if request-data
                ;; Decode gRPC message framing (5-byte header + protobuf)
                (multiple-value-bind (message-bytes compressed total-read)
                    (decode-grpc-message request-data)
                  (declare (ignore compressed total-read))
                  message-bytes)
                ;; No data - empty request
                (make-byte-array 0))))

      ;; Check if this is a server-streaming RPC (handler was stored in stream-state)
      (if (eq (server-stream-state-rpc-type stream-state) :server-streaming)
          ;; Server-streaming: create stream and invoke handler
          (let* ((handler (server-stream-state-handler stream-state))
                 (service (server-stream-state-service stream-state))
                 (method (server-stream-state-method stream-state))
                 (stream (make-grpc-server-stream
                          :connection connection
                          :stream-id stream-id
                          :headers headers
                          :recv-closed t  ; Already received the request
                          :send-closed nil
                          :request-queue (make-queue))))

            ;; Replace stream-state with grpc-server-stream
            (setf (gethash stream-id (http2-connection-active-calls connection))
                  stream)

            ;; Send initial response headers
            (server-send-initial-headers connection stream-id)

            ;; Create context
            (let ((context (make-handler-context
                            :stream-id stream-id
                            :connection connection
                            :metadata headers)))

              ;; Spawn handler thread
              (bordeaux-threads:make-thread
               (lambda () (server-handle-server-streaming-request
                           handler service method request-bytes stream context))
               :name (format nil "grpc-server-streaming-~D" stream-id))))

          ;; Unary RPC - route to handler
          (let ((path (or (cdr (assoc :path headers))
                          (cdr (assoc ":path" headers :test #'string=)))))
            (multiple-value-bind (handler service method rpc-type)
                (route-request (grpc-server-router server) path)

              ;; Create handler context
              (let ((context (make-handler-context
                              :stream-id stream-id
                              :connection connection
                              :metadata headers)))

                ;; Spawn thread to handle request (allows concurrent requests)
                (bordeaux-threads:make-thread
                 (lambda () (server-handle-request server connection stream-id
                                                  handler service method request-bytes context rpc-type))
                 :name (format nil "grpc-request-~D" stream-id)))))))))

(defun server-handle-request (server connection stream-id handler service method request-bytes context rpc-type)
  "Handle a complete gRPC request.

   Args:
     server: grpc-server
     connection: http2-connection
     stream-id: HTTP/2 stream ID
     handler: Handler instance
     service: Service name
     method: Method name
     request-bytes: Deserialized request bytes (protobuf message)
     context: handler-context
     rpc-type: RPC type (:unary, :client-streaming, :server-streaming, :bidirectional)"
  (declare (ignore server))

  (debug-log "SERVER: Calling handler service=~A method=~A rpc-type=~A request-len=~D~%"
          service method rpc-type (length request-bytes))

  (handler-case
      (ecase rpc-type
        (:unary
         ;; Unary RPC - call handler and send single response
         (multiple-value-bind (response-bytes status-code status-message response-metadata)
             (handle-unary handler service method request-bytes context)

           (debug-log "SERVER: Handler returned status=~D response-len=~D~%"
                   status-code (if response-bytes (length response-bytes) 0))

           ;; Send response
           (server-send-response connection stream-id
                                response-bytes status-code status-message
                                response-metadata)))

        ((:client-streaming :server-streaming :bidirectional)
         ;; Streaming RPC - not yet implemented
         (debug-log "SERVER: Streaming RPCs not yet implemented: ~A~%" rpc-type)
         (server-send-response connection stream-id
                              nil +grpc-status-unimplemented+
                              (format nil "~A RPC not yet implemented" rpc-type)
                              nil)))

    (error (e)
      ;; Error in handler - send INTERNAL error
      (debug-log "SERVER: Handler error: ~A~%" e)
      (server-send-response connection stream-id
                           nil +grpc-status-internal+
                           (format nil "Internal error: ~A" e)
                           nil))))

(defun server-send-response (connection stream-id response-bytes status-code status-message response-metadata)
  "Send gRPC response (headers + data + trailers).

   Args:
     connection: http2-connection
     stream-id: HTTP/2 stream ID
     response-bytes: Serialized protobuf response (or nil)
     status-code: gRPC status code
     status-message: Status message (or nil)
     response-metadata: Response metadata (or nil)"
  ;; CRITICAL: Lock is required because:
  ;; 1. HPACK encoder has mutable state (dynamic table) shared across threads
  ;; 2. Socket writes must be atomic to prevent frame interleaving
  ;; NOTE: Flush happens OUTSIDE lock to avoid blocking frame reader's ACKs
  (let ((socket (http2-connection-socket connection)))
    (bt:with-lock-held ((http2-connection-write-lock connection))
      (let ((encoder-ctx (http2-connection-hpack-encoder connection))
            (frames nil))

        ;; Build response headers frame
        (let ((headers (encode-grpc-response-headers :metadata response-metadata)))
          (let ((headers-bytes (hpack-encode-headers encoder-ctx headers)))
            (push (make-http2-frame
                   :length (length headers-bytes)
                   :type +frame-type-headers+
                   :flags +flag-end-headers+
                   :stream-id stream-id
                   :payload headers-bytes)
                  frames)))

        ;; Build DATA frame if we have response
        (when response-bytes
          (let ((grpc-message (encode-grpc-message response-bytes)))
            (push (make-http2-frame
                   :length (length grpc-message)
                   :type +frame-type-data+
                   :flags 0
                   :stream-id stream-id
                   :payload grpc-message)
                  frames)))

        ;; Build trailers frame with status
        (let ((trailers (encode-grpc-trailers status-code status-message)))
          (let ((trailers-bytes (hpack-encode-headers encoder-ctx trailers)))
            (push (make-http2-frame
                   :length (length trailers-bytes)
                   :type +frame-type-headers+
                   :flags (logior +flag-end-headers+ +flag-end-stream+)
                   :stream-id stream-id
                   :payload trailers-bytes)
                  frames)))

        ;; Send all frames to buffer (but don't flush yet)
        (dolist (frame (nreverse frames))
          (write-frame-to-stream frame socket :flush nil))))

    ;; Flush outside the write lock to avoid blocking
    (clgrpc.transport:buffered-flush socket)))
