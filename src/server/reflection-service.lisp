;;;; reflection-service.lisp - Server Reflection Service Implementation
;;;;
;;;; Implements the grpc.reflection.v1alpha.ServerReflection service

(in-package #:clgrpc.server)

;;; Reflection Service Handler

(defclass reflection-handler ()
  ((server :initarg :server
           :reader reflection-handler-server
           :documentation "The gRPC server instance for introspection"))
  (:documentation "Handler for gRPC server reflection service"))

(defun make-reflection-handler (server)
  "Create a reflection handler for the given server."
  (make-instance 'reflection-handler :server server))

;;; Bidirectional Streaming Handler

(defmethod handle-bidirectional-streaming ((handler reflection-handler)
                                           service-name method-name
                                           stream context)
  "Handle ServerReflectionInfo streaming RPC.

   This is a bidirectional streaming RPC where the client sends requests
   and the server responds with information about available services."
  (declare (ignore service-name method-name context))

  (format *error-output* "REFLECTION: Starting ServerReflectionInfo stream~%")

  (handler-case
      (loop
        ;; Receive request from client (with longer timeout for debugging)
        (let ((request-bytes (server-stream-recv stream :timeout-ms 10000)))
          (unless request-bytes
            ;; No more requests - client closed stream or timeout
            (format *error-output* "REFLECTION: No more requests (stream closed or timeout)~%")
            (return))

          (format *error-output* "REFLECTION: Received request (~D bytes)~%"
                  (length request-bytes))

          ;; Decode request
          (multiple-value-bind (host message-type)
              (clgrpc.grpc:decode-reflection-request request-bytes)

            (format *error-output* "REFLECTION: Request type=~A host=~A~%"
                    message-type host)

            ;; Handle different request types
            (let ((response-bytes
                    (ecase message-type
                      (:list-services
                       ;; List all services
                       (let ((service-names (get-registered-services
                                             (reflection-handler-server handler))))
                         (format *error-output* "REFLECTION: Listing ~D services: ~S~%"
                                 (length service-names) service-names)
                         (clgrpc.grpc:encode-reflection-response-list-services
                          host request-bytes service-names)))

                      ((:file-by-filename :file-containing-symbol)
                       ;; Not implemented yet - return empty response
                       (format *error-output* "REFLECTION: ~A not implemented~%"
                               message-type)
                       ;; TODO: Implement file descriptor responses
                       (clgrpc.grpc:encode-reflection-response-list-services
                        host request-bytes nil)))))

              ;; Send response
              (format *error-output* "REFLECTION: Sending response (~D bytes)~%"
                      (length response-bytes))
              (server-stream-send stream response-bytes)
              (format *error-output* "REFLECTION: Response sent successfully~%")))))

    (error (e)
      (format *error-output* "REFLECTION: Error in handler: ~A~%" e)
      (format *error-output* "  Error type: ~A~%" (type-of e))))

  ;; Return OK status
  (values +grpc-status-ok+ nil nil))

;;; Helper: Get all registered services

(defun get-registered-services (server)
  "Get list of all service names registered on the server.

   Returns: List of service name strings"
  (let ((router (grpc-server-router server))
        (services (make-hash-table :test 'equal)))
    ;; Iterate through all routes and collect unique service names
    (maphash (lambda (path entry)
               (declare (ignore entry))
               ;; Path format: /ServiceName/MethodName
               (multiple-value-bind (service method)
                   (clgrpc.server::parse-grpc-path path)
                 (when service
                   (setf (gethash service services) t))))
             (grpc-router-routes router))

    ;; Always include the reflection service itself
    (setf (gethash "grpc.reflection.v1.ServerReflection" services) t)

    ;; Return list of service names
    (alexandria:hash-table-keys services)))

;;; Service Registration Helper

(defun register-reflection-service (server)
  "Register the reflection service on the server.

   This should be called after all other services are registered."
  (let ((handler (make-reflection-handler server))
        (router (grpc-server-router server)))

    ;; Register the ServerReflectionInfo method
    (register-handler router
                      "grpc.reflection.v1.ServerReflection"
                      "ServerReflectionInfo"
                      handler
                      :rpc-type :bidirectional)

    (format t "Registered gRPC server reflection service~%")
    handler))
