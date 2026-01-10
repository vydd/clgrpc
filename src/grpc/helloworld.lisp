;;;; helloworld.lisp - HelloWorld protobuf messages
;;;;
;;;; CLOS-based protobuf messages for HelloWorld service

(in-package #:clgrpc.grpc)

;;; HelloRequest message

(defclass hello-request (proto-message)
  ((name
    :initarg :name
    :initform ""
    :accessor hello-request-name
    :field 1
    :proto-type :string
    :documentation "User's name"))
  (:metaclass proto-metaclass)
  (:documentation "Request message for HelloWorld.SayHello"))

(defun make-hello-request (&key (name ""))
  "Create a HelloRequest instance."
  (make-instance 'hello-request :name name))

;;; HelloReply message

(defclass hello-reply (proto-message)
  ((message
    :initarg :message
    :initform ""
    :accessor hello-reply-message
    :field 1
    :proto-type :string
    :documentation "Greeting message"))
  (:metaclass proto-metaclass)
  (:documentation "Response message for HelloWorld.SayHello"))

(defun make-hello-reply (&key (message ""))
  "Create a HelloReply instance."
  (make-instance 'hello-reply :message message))

;;; Backward compatibility functions (deprecated)

(defun encode-hello-request (name)
  "Encode HelloRequest protobuf message. DEPRECATED: Use proto-serialize instead."
  (proto-serialize (make-hello-request :name name)))

(defun decode-hello-request (bytes)
  "Decode HelloRequest protobuf message. DEPRECATED: Use proto-deserialize instead."
  (hello-request-name (proto-deserialize 'hello-request bytes)))

(defun encode-hello-reply (message)
  "Encode HelloReply protobuf message. DEPRECATED: Use proto-serialize instead."
  (proto-serialize (make-hello-reply :message message)))

(defun decode-hello-reply (bytes)
  "Decode HelloReply protobuf message. DEPRECATED: Use proto-deserialize instead."
  (hello-reply-message (proto-deserialize 'hello-reply bytes)))
