;;;; message.lisp - gRPC message encoding/decoding
;;;
;;; This module provides the interface for serializing/deserializing
;;; Protocol Buffer messages. Integration with cl-protobufs will be
;;; added in future phases.

(in-package #:clgrpc.grpc)

;;; Protocol Buffer Interface
;;;
;;; For now, we provide a simple interface that can be replaced
;;; with cl-protobufs integration later.

(defun serialize-message (message)
  "Serialize a protobuf message to bytes.

   Args:
     message: Protocol buffer message object

   Returns:
     Byte array of serialized message

   NOTE: This is a stub implementation. Integration with cl-protobufs
         will provide actual protobuf serialization."
  (cond
    ;; If message is already bytes, use as-is
    ((typep message '(simple-array (unsigned-byte 8) (*)))
     message)

    ;; If message is a string, convert to bytes
    ((stringp message)
     (babel:string-to-octets message :encoding :utf-8))

    ;; If message has a serialize method, call it
    ((and (typep message 'standard-object)
          (find-method #'serialize-message-impl nil (list (class-of message)) nil))
     (serialize-message-impl message))

    ;; Otherwise, error
    (t
     (signal-grpc-internal
      (format nil "Cannot serialize message of type ~A" (type-of message))))))

(defgeneric serialize-message-impl (message)
  (:documentation "Generic function for message-specific serialization.
                   Implement methods for custom message types."))

(defun deserialize-message (bytes message-type)
  "Deserialize a protobuf message from bytes.

   Args:
     bytes: Byte array of serialized message
     message-type: Type of message to deserialize

   Returns:
     Deserialized message object

   NOTE: This is a stub implementation. Integration with cl-protobufs
         will provide actual protobuf deserialization."
  (cond
    ;; If message-type is :bytes, return bytes as-is
    ((eq message-type :bytes)
     bytes)

    ;; If message-type is :string, convert bytes to string
    ((eq message-type :string)
     (babel:octets-to-string bytes :encoding :utf-8))

    ;; Try calling deserialize-message-impl
    ((symbolp message-type)
     (handler-case
         (deserialize-message-impl message-type bytes)
       (error ()
         ;; If no method exists, return bytes
         bytes)))

    ;; Otherwise, return bytes
    (t bytes)))

(defgeneric deserialize-message-impl (message-type bytes)
  (:documentation "Generic function for message-specific deserialization.
                   Implement methods for custom message types."))

;;; High-level Message Operations

(defun encode-grpc-request-message (message)
  "Encode a gRPC request message (serialize + frame).

   Args:
     message: Protobuf message object

   Returns:
     Byte array with gRPC framing"
  (let ((message-bytes (serialize-message message)))
    (encode-grpc-message message-bytes)))

(defun decode-grpc-request-message (framed-bytes message-type)
  "Decode a gRPC request message (unframe + deserialize).

   Args:
     framed-bytes: Byte array with gRPC framing
     message-type: Type of message to deserialize

   Returns:
     Deserialized message object"
  (multiple-value-bind (message-bytes compressed-flag bytes-read)
      (decode-grpc-message framed-bytes)
    (declare (ignore compressed-flag bytes-read))
    (deserialize-message message-bytes message-type)))

(defun encode-grpc-response-message (message)
  "Encode a gRPC response message (serialize + frame).

   Args:
     message: Protobuf message object

   Returns:
     Byte array with gRPC framing"
  (let ((message-bytes (serialize-message message)))
    (encode-grpc-message message-bytes)))

(defun decode-grpc-response-message (framed-bytes message-type)
  "Decode a gRPC response message (unframe + deserialize).

   Args:
     framed-bytes: Byte array with gRPC framing
     message-type: Type of message to deserialize

   Returns:
     Deserialized message object"
  (multiple-value-bind (message-bytes compressed-flag bytes-read)
      (decode-grpc-message framed-bytes)
    (declare (ignore compressed-flag bytes-read))
    (deserialize-message message-bytes message-type)))

;;; Test/Example Message Types
;;;
;;; Simple message types for testing without full protobuf support

(defclass simple-text-message ()
  ((text :initarg :text
         :accessor message-text
         :type string))
  (:documentation "Simple text message for testing"))

(defmethod serialize-message-impl ((message simple-text-message))
  "Serialize simple-text-message to bytes"
  (babel:string-to-octets (message-text message) :encoding :utf-8))

(defmethod deserialize-message-impl ((message-type (eql 'simple-text-message)) bytes)
  "Deserialize simple-text-message from bytes"
  (make-instance 'simple-text-message
                 :text (babel:octets-to-string bytes :encoding :utf-8)))

(defun make-simple-text-message (text)
  "Create a simple text message for testing"
  (make-instance 'simple-text-message :text text))
