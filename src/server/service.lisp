;;;; service.lisp - Service registration and management
;;;
;;; Higher-level service definition and registration

(in-package #:clgrpc.server)

;;; Service Structure

(defstruct grpc-service
  "Represents a registered gRPC service"
  (name "" :type string)
  (methods (make-hash-table :test 'equal) :type hash-table)  ; method-name -> handler
  (description "" :type string))

;;; Service Creation

(defun make-service (name &key description)
  "Create a new gRPC service.

   Args:
     name: Service name (e.g., \"helloworld.Greeter\")
     description: Optional description string

   Returns:
     grpc-service"
  (make-grpc-service
   :name name
   :description (or description "")))

(defun add-service-method (service method-name handler)
  "Add a method to a service.

   Args:
     service: grpc-service
     method-name: Method name (e.g., \"SayHello\")
     handler: Handler instance or function"
  (setf (gethash method-name (grpc-service-methods service))
        (if (functionp handler)
            (make-function-handler handler)
            handler)))

(defun remove-service-method (service method-name)
  "Remove a method from a service.

   Args:
     service: grpc-service
     method-name: Method name"
  (remhash method-name (grpc-service-methods service)))

(defun get-method-handler (service method-name)
  "Get handler for a method.

   Args:
     service: grpc-service
     method-name: Method name

   Returns:
     Handler or nil if not found"
  (gethash method-name (grpc-service-methods service)))

(defun list-methods (service)
  "List all method names in a service.

   Args:
     service: grpc-service

   Returns:
     List of method name strings"
  (alexandria:hash-table-keys (grpc-service-methods service)))

;;; Service Registration with Router

(defun register-service (router service)
  "Register all methods of a service with a router.

   Args:
     router: grpc-router
     service: grpc-service"
  (let ((service-name (grpc-service-name service)))
    (maphash (lambda (method-name handler)
               (register-handler router service-name method-name handler))
             (grpc-service-methods service))))

(defun unregister-service (router service)
  "Unregister all methods of a service from a router.

   Args:
     router: grpc-router
     service: grpc-service"
  (let ((service-name (grpc-service-name service)))
    (maphash (lambda (method-name handler)
               (declare (ignore handler))
               (unregister-handler router service-name method-name))
             (grpc-service-methods service))))

;;; Macro for Defining Services

(defmacro define-grpc-service (name &body method-specs)
  "Define a gRPC service with methods.

   Syntax:
     (define-grpc-service ServiceName
       (MethodName handler-expr [:description \"...\"])
       ...)

   Example:
     (define-grpc-service \"helloworld.Greeter\"
       (\"SayHello\" my-say-hello-handler
        :description \"Sends a greeting\")
       (\"SayGoodbye\" (lambda-handler
                        (values goodbye-response +grpc-status-ok+ nil nil))))"
  (let ((service-var (gensym "SERVICE")))
    `(let ((,service-var (make-service ,name)))
       ,@(loop for spec in method-specs
               collect (destructuring-bind (method-name handler &key description)
                           spec
                         `(add-service-method ,service-var ,method-name
                                            ,(if description
                                                 `(progn
                                                    ;; Store description somewhere if needed
                                                    ,handler)
                                                 handler))))
       ,service-var)))
