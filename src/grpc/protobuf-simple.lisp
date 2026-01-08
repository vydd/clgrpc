;;;; protobuf-simple.lisp - Simple protobuf encoding for basic messages
;;;;
;;;; This is a minimal protobuf implementation for simple string messages.
;;;; For production use, integrate cl-protobufs or another full implementation.
;;;;
;;;; Wire format: https://protobuf.dev/programming-guides/encoding/

(in-package #:clgrpc.grpc)

;;; Protobuf Wire Types
(defconstant +wire-type-varint+ 0)
(defconstant +wire-type-64bit+ 1)
(defconstant +wire-type-length-delimited+ 2)
(defconstant +wire-type-start-group+ 3)
(defconstant +wire-type-end-group+ 4)
(defconstant +wire-type-32bit+ 5)

;;; Varint Encoding

(defun encode-varint (value)
  "Encode unsigned integer as varint. Returns byte array."
  (let ((result (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop
      (let ((byte (logand value #x7F)))
        (setf value (ash value -7))
        (if (zerop value)
            (progn
              (vector-push-extend byte result)
              (return result))
            (vector-push-extend (logior byte #x80) result))))))

(defun decode-varint (bytes &optional (offset 0))
  "Decode varint from byte array. Returns (values value new-offset)."
  (let ((result 0)
        (shift 0)
        (pos offset))
    (loop
      (when (>= pos (length bytes))
        (error "Incomplete varint"))
      (let ((byte (aref bytes pos)))
        (incf pos)
        (setf result (logior result (ash (logand byte #x7F) shift)))
        (incf shift 7)
        (when (zerop (logand byte #x80))
          (return (values result pos)))))))

;;; Field Encoding

(defun encode-field-tag (field-number wire-type)
  "Encode field tag (field number + wire type)."
  (encode-varint (logior (ash field-number 3) wire-type)))

(defun decode-field-tag (bytes offset)
  "Decode field tag. Returns (values field-number wire-type new-offset)."
  (multiple-value-bind (tag new-offset)
      (decode-varint bytes offset)
    (values (ash tag -3) (logand tag #x7) new-offset)))

(defun encode-string-field (field-number string)
  "Encode a string field. Returns byte array."
  (let* ((string-bytes (babel:string-to-octets string :encoding :utf-8))
         (tag (encode-field-tag field-number +wire-type-length-delimited+))
         (length (encode-varint (length string-bytes)))
         (result (make-array (+ (length tag) (length length) (length string-bytes))
                            :element-type '(unsigned-byte 8))))
    (replace result tag)
    (replace result length :start1 (length tag))
    (replace result string-bytes :start1 (+ (length tag) (length length)))
    result))

(defun decode-string-field (bytes offset)
  "Decode a length-delimited string field. Returns (values string new-offset)."
  (multiple-value-bind (length new-offset)
      (decode-varint bytes offset)
    (let ((string-bytes (subseq bytes new-offset (+ new-offset length))))
      (values (babel:octets-to-string string-bytes :encoding :utf-8)
              (+ new-offset length)))))

;;; HelloWorld Message Encoding
;;;
;;; HelloRequest { string name = 1; }
;;; HelloReply { string message = 1; }

(defun encode-hello-request (name)
  "Encode HelloRequest message.

   Args:
     name: String name field

   Returns:
     Byte array with encoded protobuf message"
  (encode-string-field 1 name))

(defun decode-hello-request (bytes)
  "Decode HelloRequest message.

   Args:
     bytes: Encoded protobuf message

   Returns:
     Name string"
  (let ((offset 0)
        (name ""))
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
                  ;; Skip unknown field
                  (error "Unknown field ~D with wire type ~D" field-number wire-type)))))
    name))

(defun encode-hello-reply (message)
  "Encode HelloReply message.

   Args:
     message: String message field

   Returns:
     Byte array with encoded protobuf message"
  (encode-string-field 1 message))

(defun decode-hello-reply (bytes)
  "Decode HelloReply message.

   Args:
     bytes: Encoded protobuf message

   Returns:
     Message string"
  (let ((offset 0)
        (message ""))
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
                  ;; Skip unknown field
                  (error "Unknown field ~D with wire type ~D" field-number wire-type)))))
    message))
