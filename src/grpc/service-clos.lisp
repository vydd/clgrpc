;;;; service-clos.lisp - CLOS-based gRPC service definition
;;;;
;;;; Provides a metaclass for defining gRPC services as CLOS classes
;;;; with automatic method registration and routing, similar to proto-clos.

(in-package #:clgrpc.grpc)

;; Ensure closer-mop is available at compile time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :closer-mop))

;;; Method Metadata Structure

(defstruct grpc-method-info
  "Metadata about a gRPC method"
  (lisp-name nil :type symbol)           ; Lisp function name (e.g., say-hello)
  (grpc-name nil :type string)           ; gRPC method name (e.g., "SayHello")
  (rpc-type :unary :type keyword)        ; :unary, :client-streaming, :server-streaming, :bidirectional
  (request-type nil :type symbol)        ; Request message class name
  (response-type nil :type symbol)       ; Response message class name (for unary/server-streaming)
  (documentation nil :type (or null string)))

;;; Service Metaclass

(defclass grpc-service-metaclass (closer-mop:standard-class)
  ((service-name
    :initarg :service-name
    :initform nil
    :reader %service-name
    :documentation "Fully qualified gRPC service name (e.g., 'helloworld.Greeter')")
   (package-name
    :initarg :package
    :initform nil
    :reader %service-package-name
    :documentation "Protobuf package name (e.g., 'helloworld')")
   (methods
    :initform (make-hash-table :test 'eq)
    :reader %service-methods
    :documentation "Hash table mapping lisp method names to grpc-method-info"))
  (:documentation "Metaclass for gRPC service classes"))

;; Explicit accessor methods for metaclass slots
(defmethod service-name ((metaclass grpc-service-metaclass))
  (slot-value metaclass 'service-name))

(defmethod service-package-name ((metaclass grpc-service-metaclass))
  (slot-value metaclass 'package-name))

(defmethod service-methods ((metaclass grpc-service-metaclass))
  (slot-value metaclass 'methods))

;; Allow grpc-service-metaclass to be a metaclass
(defmethod closer-mop:validate-superclass ((class grpc-service-metaclass)
                                           (superclass closer-mop:standard-class))
  t)

;;; Base Class for gRPC Services

(defclass grpc-service ()
  ()
  (:metaclass grpc-service-metaclass)
  (:documentation "Base class for all gRPC services"))

;;; Method Registration

(defun register-grpc-method (service-class lisp-name grpc-name rpc-type request-type response-type documentation)
  "Register a gRPC method on a service class.
   service-class should be an instance of grpc-service-metaclass (i.e., a class object)"
  ;; service-class is already an instance of grpc-service-metaclass
  (let* ((methods (service-methods service-class))
         (info (make-grpc-method-info
                :lisp-name lisp-name
                :grpc-name grpc-name
                :rpc-type rpc-type
                :request-type request-type
                :response-type response-type
                :documentation documentation)))
    (setf (gethash lisp-name methods) info)
    info))

(defun find-grpc-method (service-class lisp-name)
  "Find method metadata by Lisp name"
  (let ((methods (service-methods service-class)))
    (gethash lisp-name methods)))

(defun find-grpc-method-by-name (service-class grpc-name)
  "Find method metadata by gRPC name (string)"
  (let ((methods (service-methods service-class)))
    (loop for info being the hash-values of methods
          when (string= (grpc-method-info-grpc-name info) grpc-name)
          return info)))

(defun list-grpc-methods (service-class)
  "List all gRPC methods defined for a service class"
  (let ((methods (service-methods service-class))
        (result nil))
    (maphash (lambda (name info)
               (declare (ignore name))
               (push info result))
             methods)
    (nreverse result)))

;;; Helper: Convert kebab-case to CamelCase

(defun kebab-to-camel-case (symbol)
  "Convert a kebab-case symbol to CamelCase string.

   Examples:
     say-hello → SayHello
     get-feature → GetFeature
     list-features → ListFeatures"
  (let ((name (string-downcase (symbol-name symbol))))
    (with-output-to-string (out)
      (loop with capitalize-next = t
            for char across name
            do (cond
                 ((char= char #\-)
                  (setf capitalize-next t))
                 (capitalize-next
                  (write-char (char-upcase char) out)
                  (setf capitalize-next nil))
                 (t
                  (write-char char out)))))))

;;; Method Definition Macro

(defmacro defgrpc-method (lisp-name lambda-list &body body)
  "Define a gRPC method.

  Lambda list must be ((service service-class) (request request-class) context)

  Body can start with optional keyword options:
    :method-name \"GrpcMethodName\"  (optional, defaults to CamelCase of lisp-name)
    :rpc-type :unary | :client-streaming | :server-streaming | :bidirectional (default: :unary)
    :response-type response-class  (optional, inferred for unary/server-streaming)
    :documentation \"Method documentation\"

  Examples:
    ;; Minimal - method-name defaults to \"SayHello\", rpc-type defaults to :unary
    (defgrpc-method say-hello ((service greeter-service)
                               (request hello-request)
                               context)
      (make-hello-reply :message (format nil \"Hello ~A!\" (hello-request-name request))))

    ;; Explicit
    (defgrpc-method say-hello ((service greeter-service)
                               (request hello-request)
                               context)
      (:method-name \"SayHello\")
      (:rpc-type :unary)
      (make-hello-reply :message (format nil \"Hello ~A!\" (hello-request-name request))))"

  ;; Parse lambda list
  (unless (and (= (length lambda-list) 3)
               (listp (first lambda-list))
               (listp (second lambda-list)))
    (error "Lambda list must be ((service service-class) (request request-class) context)"))

  (let* ((service-binding (first (first lambda-list)))
         (service-class (second (first lambda-list)))
         (request-binding (first (second lambda-list)))
         (request-class (second (second lambda-list)))
         (context-binding (third lambda-list)))

    ;; Parse keyword options from body
    (let ((grpc-name nil)
          (rpc-type :unary)
          (response-type nil)
          (doc nil)
          (method-body nil))

      (loop for form in body
            do (cond
                 ((and (listp form) (eq (first form) :method-name))
                  (setf grpc-name (second form)))
                 ((and (listp form) (eq (first form) :rpc-type))
                  (setf rpc-type (second form)))
                 ((and (listp form) (eq (first form) :response-type))
                  (setf response-type (second form)))
                 ((and (listp form) (eq (first form) :documentation))
                  (setf doc (second form)))
                 (t
                  (push form method-body))))

      (setf method-body (nreverse method-body))

      ;; Apply defaults
      (unless grpc-name
        (setf grpc-name (kebab-to-camel-case lisp-name)))

      ;; Validate
      (unless (member rpc-type '(:unary :client-streaming :server-streaming :bidirectional))
        (error "defgrpc-method ~A: invalid :rpc-type ~A" lisp-name rpc-type))

      ;; Generate code
      `(progn
         ;; Define the generic function if it doesn't exist
         (unless (fboundp ',lisp-name)
           (defgeneric ,lisp-name (service request context)
             ,@(when doc `((:documentation ,doc)))))

         ;; Define the method specializing on service and request classes
         (defmethod ,lisp-name ((,service-binding ,service-class)
                                (,request-binding ,request-class)
                                ,context-binding)
           ,@(when doc `(,doc))
           ,@method-body)

         ;; Register method metadata with the service class
         (eval-when (:load-toplevel :execute)
           (register-grpc-method (find-class ',service-class)
                                 ',lisp-name
                                 ,grpc-name
                                 ,rpc-type
                                 ',request-class
                                 ',response-type
                                 ,doc))))))

;;; Helper: Get service name from instance or class

(defun get-service-name (service-or-class)
  "Get the gRPC service name from a service instance or class"
  (let ((service-class (if (typep service-or-class 'grpc-service)
                           ;; It's an instance, get its class
                           (class-of service-or-class)
                           ;; It's already a class
                           service-or-class)))
    (service-name service-class)))

(defun get-service-package (service-or-class)
  "Get the protobuf package name from a service instance or class"
  (let ((service-class (if (typep service-or-class 'grpc-service)
                           ;; It's an instance, get its class
                           (class-of service-or-class)
                           ;; It's already a class
                           service-or-class)))
    (service-package-name service-class)))
