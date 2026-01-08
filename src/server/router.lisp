;;;; router.lisp - Request routing for gRPC server
;;;
;;; Routes incoming gRPC requests to registered handlers

(in-package #:clgrpc.server)

;;; Router Structure

(defstruct grpc-router
  "Routes requests to handlers"
  (routes (make-hash-table :test 'equal) :type hash-table)  ; path -> handler
  (default-handler (make-instance 'default-handler))
  (lock (bordeaux-threads:make-lock "router-lock")))

(defun make-router (&key default-handler)
  "Create a new router.

   Args:
     default-handler: Handler to use when no route matches (optional)"
  (make-grpc-router
   :default-handler (or default-handler (make-instance 'default-handler))))

;;; Route Registration

(defun register-handler (router service-name method-name handler)
  "Register a handler for a specific service/method.

   Args:
     router: grpc-router
     service-name: Service name (e.g., \"helloworld.Greeter\")
     method-name: Method name (e.g., \"SayHello\")
     handler: Handler instance

   Example:
     (register-handler router \"helloworld.Greeter\" \"SayHello\" my-handler)"
  (let ((path (make-grpc-path service-name method-name)))
    (bordeaux-threads:with-lock-held ((grpc-router-lock router))
      (setf (gethash path (grpc-router-routes router)) handler))))

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
     (values handler service-name method-name)"
  (multiple-value-bind (service method)
      (parse-grpc-path path)
    (if (and service method)
        (bordeaux-threads:with-lock-held ((grpc-router-lock router))
          (let ((handler (gethash path (grpc-router-routes router))))
            (values (or handler (grpc-router-default-handler router))
                    service
                    method)))
        ;; Invalid path - return default handler with nil names
        (values (grpc-router-default-handler router) nil nil))))

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
