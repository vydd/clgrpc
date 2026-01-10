;;;; router.lisp - Request routing for gRPC server
;;;
;;; Routes incoming gRPC requests to registered handlers

(in-package #:clgrpc.server)

;;; Router Structure

(defstruct route-entry
  "Represents a registered route"
  (handler nil)                         ; Handler instance
  (rpc-type :unary :type keyword))     ; :unary, :client-streaming, :server-streaming, :bidirectional

(defstruct grpc-router
  "Routes requests to handlers"
  (routes (make-hash-table :test 'equal) :type hash-table)  ; path -> route-entry
  (default-handler (make-instance 'default-handler))
  (lock (bordeaux-threads:make-lock "router-lock")))

(defun make-router (&key default-handler)
  "Create a new router.

   Args:
     default-handler: Handler to use when no route matches (optional)"
  (make-grpc-router
   :default-handler (or default-handler (make-instance 'default-handler))))

;;; Route Registration

(defun register-handler (router service-name method-name handler &key (rpc-type :unary))
  "Register a handler for a specific service/method.

   Args:
     router: grpc-router
     service-name: Service name (e.g., \"helloworld.Greeter\")
     method-name: Method name (e.g., \"SayHello\")
     handler: Handler instance
     rpc-type: RPC type - :unary, :client-streaming, :server-streaming, :bidirectional

   Example:
     (register-handler router \"helloworld.Greeter\" \"SayHello\" my-handler)
     (register-handler router \"helloworld.Greeter\" \"StreamHellos\" my-handler
                      :rpc-type :server-streaming)"
  (let ((path (make-grpc-path service-name method-name)))
    (bordeaux-threads:with-lock-held ((grpc-router-lock router))
      (setf (gethash path (grpc-router-routes router))
            (make-route-entry :handler handler :rpc-type rpc-type)))))

(defun unregister-handler (router service-name method-name)
  "Unregister a handler for a service/method.

   Args:
     router: grpc-router
     service-name: Service name
     method-name: Method name"
  (let ((path (make-grpc-path service-name method-name)))
    (bordeaux-threads:with-lock-held ((grpc-router-lock router))
      (remhash path (grpc-router-routes router)))))

(defun make-grpc-path (service-name method-name)
  "Create gRPC path from service and method names.

   gRPC path format: /ServiceName/MethodName

   Example:
     (make-grpc-path \"helloworld.Greeter\" \"SayHello\")
     => \"/helloworld.Greeter/SayHello\""
  (format nil "/~A/~A" service-name method-name))

(defun parse-grpc-path (path)
  "Parse gRPC path into service and method names.

   Args:
     path: gRPC path string (e.g., \"/helloworld.Greeter/SayHello\")

   Returns:
     (values service-name method-name) or (values nil nil) if invalid"
  (when (and (stringp path)
             (> (length path) 2)
             (char= (char path 0) #\/))
    (let* ((rest (subseq path 1))
           (slash-pos (position #\/ rest)))
      (when slash-pos
        (values (subseq rest 0 slash-pos)
                (subseq rest (1+ slash-pos)))))))

;;; Request Routing

(defun route-request (router path)
  "Find handler for a request path.

   Args:
     router: grpc-router
     path: gRPC path string (from :path pseudo-header)

   Returns:
     (values handler service-name method-name rpc-type)"
  (multiple-value-bind (service method)
      (parse-grpc-path path)
    (if (and service method)
        (bordeaux-threads:with-lock-held ((grpc-router-lock router))
          (let ((entry (gethash path (grpc-router-routes router))))
            (if entry
                (values (route-entry-handler entry)
                        service
                        method
                        (route-entry-rpc-type entry))
                ;; Not found - return default handler with unary type
                (values (grpc-router-default-handler router)
                        service
                        method
                        :unary))))
        ;; Invalid path - return default handler with nil names
        (values (grpc-router-default-handler router) nil nil :unary))))

;;; Bulk Registration

(defun register-service-handlers (router service-name handler-map)
  "Register multiple handlers for a service at once.

   Args:
     router: grpc-router
     service-name: Service name
     handler-map: Alist of (method-name . handler) pairs

   Example:
     (register-service-handlers router \"helloworld.Greeter\"
       '((\"SayHello\" . say-hello-handler)
         (\"SayGoodbye\" . say-goodbye-handler)))"
  (dolist (entry handler-map)
    (destructuring-bind (method-name . handler) entry
      (register-handler router service-name method-name handler))))
