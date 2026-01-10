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

;;; Streaming Handler Protocols

(defgeneric handle-client-streaming (handler service-name method-name stream context)
  (:documentation "Handle a client streaming RPC (client sends many → server sends one).

   Args:
     handler: Handler instance
     service-name: Service name string
     method-name: Method name string
     stream: grpc-server-stream for receiving messages
     context: handler-context with request metadata

   Returns:
     (values response-bytes status-code status-message response-metadata)

   Handler should:
     - Call (server-stream-recv stream) repeatedly to receive messages
     - Return single response when done

   Example:
     (defmethod handle-client-streaming ((handler my-handler) service method stream ctx)
       (let ((messages nil))
         (loop for msg = (server-stream-recv stream)
               while msg
               do (push msg messages))
         (let ((response (process-all-messages (nreverse messages))))
           (values response +grpc-status-ok+ nil nil))))"))

(defgeneric handle-server-streaming (handler service-name method-name request-bytes stream context)
  (:documentation "Handle a server streaming RPC (client sends one → server sends many).

   Args:
     handler: Handler instance
     service-name: Service name string
     method-name: Method name string
     request-bytes: Single request message (serialized protobuf)
     stream: grpc-server-stream for sending responses
     context: handler-context with request metadata

   Returns:
     (values status-code status-message response-metadata)

   Handler should:
     - Process the single request-bytes
     - Call (server-stream-send stream response-bytes) for each response
     - Return status when done (do NOT close stream, server will do that)

   Example:
     (defmethod handle-server-streaming ((handler my-handler) service method req stream ctx)
       (let ((items (process-request req)))
         (dolist (item items)
           (server-stream-send stream (serialize-item item)))
         (values +grpc-status-ok+ nil nil)))"))

(defgeneric handle-bidirectional-streaming (handler service-name method-name stream context)
  (:documentation "Handle a bidirectional streaming RPC (both send many).

   Args:
     handler: Handler instance
     service-name: Service name string
     method-name: Method name string
     stream: grpc-server-stream for both sending and receiving
     context: handler-context with request metadata

   Returns:
     (values status-code status-message response-metadata)

   Handler should:
     - Call (server-stream-recv stream) to receive messages
     - Call (server-stream-send stream response-bytes) to send responses
     - Can interleave receives and sends in any order
     - Return status when done

   Example:
     (defmethod handle-bidirectional-streaming ((handler my-handler) service method stream ctx)
       (loop for msg = (server-stream-recv stream :timeout-ms 1000)
             while msg
             do (let ((response (process-message msg)))
                  (server-stream-send stream response)))
       (values +grpc-status-ok+ nil nil)))"))

;;; Default implementations for streaming (return UNIMPLEMENTED)

(defmethod handle-client-streaming ((handler default-handler) service-name method-name stream context)
  (declare (ignore stream context))
  (values nil
          +grpc-status-unimplemented+
          (format nil "Method ~A/~A not implemented" service-name method-name)
          nil))

(defmethod handle-server-streaming ((handler default-handler) service-name method-name request-bytes stream context)
  (declare (ignore request-bytes stream context))
  (values +grpc-status-unimplemented+
          (format nil "Method ~A/~A not implemented" service-name method-name)
          nil))

(defmethod handle-bidirectional-streaming ((handler default-handler) service-name method-name stream context)
  (declare (ignore stream context))
  (values +grpc-status-unimplemented+
          (format nil "Method ~A/~A not implemented" service-name method-name)
          nil))
