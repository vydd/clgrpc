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

;;; CLOS Service Registration

(defun register-service (router service-instance)
  "Register all methods from a CLOS-based gRPC service.

   This function automatically discovers all methods defined with defgrpc-method
   on the service class and registers handlers for each one.

   Args:
     router: grpc-router
     service-instance: Instance of a class with grpc-service-metaclass

   Example:
     (defclass greeter-service (grpc-service)
       ()
       (:metaclass grpc-service-metaclass)
       (:service-name \"helloworld.Greeter\"))

     (defgrpc-method say-hello ((service greeter-service)
                                (request hello-request)
                                context)
       (:method-name \"SayHello\")
       (:rpc-type :unary)
       (make-hello-reply :message \"Hello!\"))

     (register-service router (make-instance 'greeter-service))"

  (unless (typep service-instance 'clgrpc.grpc:grpc-service)
    (error "Service instance must be a subclass of grpc-service"))

  (let* ((service-class (class-of service-instance))
         (service-name (clgrpc.grpc:get-service-name service-instance))
         (methods (clgrpc.grpc:list-grpc-methods service-class)))

    (unless service-name
      (error "Service class must have :service-name defined in metaclass options"))

    ;; Register each method
    (dolist (method-info methods)
      (let* ((lisp-name (clgrpc.grpc:grpc-method-info-lisp-name method-info))
             (grpc-name (clgrpc.grpc:grpc-method-info-grpc-name method-info))
             (rpc-type (clgrpc.grpc:grpc-method-info-rpc-type method-info))
             (request-type (clgrpc.grpc:grpc-method-info-request-type method-info))
             ;; Create a wrapper handler that deserializes/serializes
             (wrapper-handler (make-clos-method-handler service-instance
                                                        lisp-name
                                                        request-type
                                                        rpc-type)))

        (register-handler router service-name grpc-name wrapper-handler
                         :rpc-type rpc-type)

        (format t "Registered ~A ~A/~A~%"
                rpc-type service-name grpc-name)))))

(defun make-clos-method-handler (service-instance method-name request-type rpc-type)
  "Create a handler wrapper that calls a CLOS method with proper serialization.

   Returns a handler object that implements handle-unary, handle-client-streaming, etc."

  (let ((handler (make-instance 'clos-method-handler
                               :service service-instance
                               :method-name method-name
                               :request-type request-type
                               :rpc-type rpc-type)))
    handler))

;;; CLOS Method Handler

(defclass clos-method-handler ()
  ((service
    :initarg :service
    :reader handler-service
    :documentation "Service instance")
   (method-name
    :initarg :method-name
    :reader handler-method-name
    :documentation "Lisp method name (symbol)")
   (request-type
    :initarg :request-type
    :reader handler-request-type
    :documentation "Request message class name (symbol)")
   (rpc-type
    :initarg :rpc-type
    :reader handler-rpc-type
    :documentation "RPC type (:unary, :client-streaming, etc.)"))
  (:documentation "Handler wrapper for CLOS-defined gRPC methods"))

;; Implement handler methods for each RPC type
(defmethod handle-unary ((handler clos-method-handler) service-name method-name request-bytes context)
  (declare (ignore service-name method-name))

  ;; Deserialize request
  (let* ((request (clgrpc.grpc:proto-deserialize (handler-request-type handler) request-bytes))
         (service (handler-service handler))
         (lisp-method (handler-method-name handler)))

    ;; Call the user's method
    (let ((response (funcall lisp-method service request context)))
      ;; Serialize response
      (values (clgrpc.grpc:proto-serialize response)
              clgrpc.grpc:+grpc-status-ok+
              nil
              nil))))

(defmethod handle-server-streaming ((handler clos-method-handler)
                                     service-name method-name
                                     request-bytes stream context)
  (declare (ignore service-name method-name))

  ;; Add stream to context
  (setf (handler-context-grpc-stream context) stream)

  ;; Deserialize request
  (let* ((request (clgrpc.grpc:proto-deserialize (handler-request-type handler) request-bytes))
         (service (handler-service handler))
         (lisp-method (handler-method-name handler)))

    ;; Call the user's method - it will use (get-stream context) to access stream
    (funcall lisp-method service request context)))

(defmethod handle-client-streaming ((handler clos-method-handler)
                                     service-name method-name
                                     stream context)
  (declare (ignore service-name method-name))

  ;; Add stream to context
  (setf (handler-context-grpc-stream context) stream)

  (let* ((service (handler-service handler))
         (lisp-method (handler-method-name handler))
         (request-type (handler-request-type handler)))

    ;; Create a dummy request instance (method will use stream from context instead)
    (let ((dummy-request (make-instance request-type)))
      ;; Call user's method - it will use (get-stream context) to receive messages
      (funcall lisp-method service dummy-request context))))

(defmethod handle-bidirectional-streaming ((handler clos-method-handler)
                                            service-name method-name
                                            stream context)
  (declare (ignore service-name method-name))

  ;; Add stream to context
  (setf (handler-context-grpc-stream context) stream)

  (let* ((service (handler-service handler))
         (lisp-method (handler-method-name handler))
         (request-type (handler-request-type handler)))

    ;; Create a dummy request instance (method will use stream from context instead)
    (let ((dummy-request (make-instance request-type)))
      ;; Call user's method - it will use (get-stream context) for send/recv
      (funcall lisp-method service dummy-request context))))
