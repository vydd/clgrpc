;;;; handler.lisp - gRPC request handler interface
;;;
;;; Defines the handler interface for processing gRPC requests

(in-package #:clgrpc.server)

;;; Handler Context

(defstruct handler-context
  "Context passed to RPC handlers"
  (stream-id nil :type (or null fixnum))
  (connection nil)
  (metadata nil :type list)        ; Request metadata (headers)
  (deadline nil :type (or null fixnum))  ; Request deadline (internal time)
  (cancelled nil :type boolean))

;;; Unary Handler Protocol

(defgeneric handle-unary (handler service-name method-name request-bytes context)
  (:documentation "Handle a unary RPC call.

   Args:
     handler: Handler instance
     service-name: Service name string (e.g., \"helloworld.Greeter\")
     method-name: Method name string (e.g., \"SayHello\")
     request-bytes: Serialized protobuf request (raw bytes)
     context: handler-context with request metadata

   Returns:
     (values response-bytes status-code status-message response-metadata)
     - response-bytes: Serialized protobuf response (or nil on error)
     - status-code: gRPC status code (0 = OK)
     - status-message: Status message string (nil for OK)
     - response-metadata: List of (name . value) pairs (optional)

   Signals:
     May signal grpc-error to return non-OK status"))

;;; Default Handler (returns UNIMPLEMENTED)

(defclass default-handler ()
  ()
  (:documentation "Default handler that returns UNIMPLEMENTED for all methods"))

(defmethod handle-unary ((handler default-handler) service-name method-name request-bytes context)
  (declare (ignore request-bytes context))
  (values nil
          +grpc-status-unimplemented+
          (format nil "Method ~A/~A not implemented" service-name method-name)
          nil))

;;; Function Handler (simple wrapper for function handlers)

(defclass function-handler ()
  ((handler-fn :initarg :handler-fn
               :reader handler-function
               :type function
               :documentation "Function to call for handling request"))
  (:documentation "Handler that delegates to a function"))

(defmethod handle-unary ((handler function-handler) service-name method-name request-bytes context)
  (funcall (handler-function handler) service-name method-name request-bytes context))

(defun make-function-handler (fn)
  "Create a handler that delegates to a function.

   Function signature: (lambda (service method request-bytes context) ...)"
  (make-instance 'function-handler :handler-fn fn))

;;; Lambda Handler (inline handler)

(defmacro lambda-handler (&body body)
  "Create an inline handler using lambda syntax.

   Example:
     (lambda-handler
       (let ((request (deserialize request-bytes)))
         (let ((response (process request)))
           (values (serialize response) +grpc-status-ok+ nil nil))))"
  `(make-function-handler
    (lambda (service-name method-name request-bytes context)
      (declare (ignorable service-name method-name context))
      ,@body)))
