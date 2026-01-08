;;;; protobuf.lisp - Repeated fields and embedded messages
;;;;
;;;; Extends protobuf-simple.lisp with support for:
;;;; - Repeated fields (packed and unpacked)
;;;; - Embedded messages
;;;; - Message encoding/decoding framework

(in-package #:clgrpc.grpc)

;;; Repeated Fields

(defun encode-repeated-field (field-number values encode-fn)
  "Encode repeated field (unpacked). Each value encoded separately.

   Args:
     field-number: Field number
     values: List of values
     encode-fn: Function to encode single field (field-number value) -> bytes

   Returns:
     Concatenated byte array"
  (if (null values)
      (make-array 0 :element-type '(unsigned-byte 8))
      (apply #'concatenate '(vector (unsigned-byte 8))
             (mapcar (lambda (v) (funcall encode-fn field-number v))
                     values))))

(defun encode-packed-repeated-field (field-number values encode-value-fn)
  "Encode packed repeated field (length-delimited).

   Args:
     field-number: Field number
     values: List of values
     encode-value-fn: Function to encode value (value) -> bytes (no tag)

   Returns:
     Byte array with tag + length + concatenated values"
  (if (null values)
      (make-array 0 :element-type '(unsigned-byte 8))
      (let* ((encoded-values (apply #'concatenate '(vector (unsigned-byte 8))
                                   (mapcar encode-value-fn values)))
             (tag (encode-field-tag field-number +wire-type-length-delimited+))
             (length (encode-varint (length encoded-values)))
             (result (make-array (+ (length tag) (length length) (length encoded-values))
                                :element-type '(unsigned-byte 8))))
        (replace result tag)
        (replace result length :start1 (length tag))
        (replace result encoded-values :start1 (+ (length tag) (length length)))
        result)))

;; Convenience functions for packed repeated numeric types

(defun encode-packed-int32 (field-number values)
  "Encode packed repeated int32."
  (encode-packed-repeated-field field-number values #'encode-varint))

(defun encode-packed-int64 (field-number values)
  "Encode packed repeated int64."
  (encode-packed-repeated-field field-number values #'encode-varint))

(defun encode-packed-sint32 (field-number values)
  "Encode packed repeated sint32."
  (encode-packed-repeated-field field-number values
                                (lambda (v) (encode-varint (encode-zigzag-32 v)))))

(defun encode-packed-sint64 (field-number values)
  "Encode packed repeated sint64."
  (encode-packed-repeated-field field-number values
                                (lambda (v) (encode-varint (encode-zigzag-64 v)))))

(defun encode-packed-fixed32 (field-number values)
  "Encode packed repeated fixed32."
  (encode-packed-repeated-field field-number values #'encode-fixed32))

(defun encode-packed-fixed64 (field-number values)
  "Encode packed repeated fixed64."
  (encode-packed-repeated-field field-number values #'encode-fixed64))

(defun encode-packed-float (field-number values)
  "Encode packed repeated float."
  (encode-packed-repeated-field field-number values #'pb-encode-float))

(defun encode-packed-double (field-number values)
  "Encode packed repeated double."
  (encode-packed-repeated-field field-number values #'pb-encode-double))

;;; Embedded Messages

(defun encode-message-field (field-number message-bytes)
  "Encode embedded message field.

   Args:
     field-number: Field number
     message-bytes: Already-encoded message bytes

   Returns:
     Byte array with tag + length + message"
  (let* ((tag (encode-field-tag field-number +wire-type-length-delimited+))
         (length (encode-varint (length message-bytes)))
         (result (make-array (+ (length tag) (length length) (length message-bytes))
                            :element-type '(unsigned-byte 8))))
    (replace result tag)
    (replace result length :start1 (length tag))
    (replace result message-bytes :start1 (+ (length tag) (length length)))
    result))

(defun decode-message-field (bytes offset)
  "Decode embedded message field.

   Returns:
     (values message-bytes new-offset)"
  (decode-bytes-field bytes offset))

;;; Field Skipping (for forward compatibility)

(defun skip-field (bytes offset wire-type)
  "Skip unknown field. Returns new offset.

   This allows messages to be forward-compatible with newer versions."
  (case wire-type
    (#.+wire-type-varint+
     ;; Skip varint
     (multiple-value-bind (value new-offset)
         (decode-varint bytes offset)
       (declare (ignore value))
       new-offset))

    (#.+wire-type-64bit+
     ;; Skip 8 bytes
     (+ offset 8))

    (#.+wire-type-length-delimited+
     ;; Skip length-delimited data
     (multiple-value-bind (length new-offset)
         (decode-varint bytes offset)
       (+ new-offset length)))

    (#.+wire-type-32bit+
     ;; Skip 4 bytes
     (+ offset 4))

    (otherwise
     (error "Cannot skip unknown wire type ~D" wire-type))))

;;; Message Encoding/Decoding Framework

(defun encode-message (fields)
  "Encode a complete message from list of field byte arrays.

   Args:
     fields: List of encoded field byte arrays

   Returns:
     Concatenated byte array"
  (if (null fields)
      (make-array 0 :element-type '(unsigned-byte 8))
      (apply #'concatenate '(vector (unsigned-byte 8)) fields)))

(defun decode-message-with-schema (bytes field-decoders)
  "Decode message using schema (field decoders).

   Args:
     bytes: Encoded message bytes
     field-decoders: Hash table mapping field-number -> (lambda (bytes offset wire-type) ...)

   Returns:
     Hash table of field-number -> decoded-value"
  (let ((result (make-hash-table :test 'eql))
        (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (decode-field-tag bytes offset)
               (setf offset new-offset)

               (let ((decoder (gethash field-number field-decoders)))
                 (if decoder
                     ;; Decode known field
                     (multiple-value-bind (value new-offset)
                         (funcall decoder bytes offset wire-type)
                       (setf (gethash field-number result) value)
                       (setf offset new-offset))
                     ;; Skip unknown field (forward compatibility)
                     (setf offset (skip-field bytes offset wire-type))))))
    result))
