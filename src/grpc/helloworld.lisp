;;;; helloworld.lisp - HelloWorld protobuf messages for testing
;;;;
;;;; Generated from helloworld.proto with proper naming

(in-package #:clgrpc.grpc)

;;; HelloRequest message

(defun encode-hello-request (name)
  "Encode HelloRequest protobuf message.

   Args:
     name: String - user's name

   Returns:
     Byte array with encoded message"
  (encode-message
   (list
    (encode-string-field 1 name))))

(defun decode-hello-request (bytes)
  "Decode HelloRequest protobuf message.

   Args:
     bytes: Byte array with encoded message

   Returns:
     name (string)"
  (let ((name "") (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (value new-offset)
                      (decode-string-field bytes offset)
                    (setf name value)
                    (setf offset new-offset)))
                 (t
                  (setf offset (skip-field bytes offset wire-type))))))
    (values name)))

;;; HelloReply message

(defun encode-hello-reply (message)
  "Encode HelloReply protobuf message.

   Args:
     message: String - greeting message

   Returns:
     Byte array with encoded message"
  (encode-message
   (list
    (encode-string-field 1 message))))

(defun decode-hello-reply (bytes)
  "Decode HelloReply protobuf message.

   Args:
     bytes: Byte array with encoded message

   Returns:
     message (string)"
  (let ((message "") (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (value new-offset)
                      (decode-string-field bytes offset)
                    (setf message value)
                    (setf offset new-offset)))
                 (t
                  (setf offset (skip-field bytes offset wire-type))))))
    (values message)))
