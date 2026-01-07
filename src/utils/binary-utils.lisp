;;;; binary-utils.lisp - Binary/byte array manipulation utilities

(in-package #:clgrpc.utils)

(defun make-byte-array (size &key (initial-element 0))
  "Create a byte array of specified size."
  (make-array size :element-type '(unsigned-byte 8)
                   :initial-element initial-element))

(defun copy-bytes (source dest &key (start1 0) (start2 0) (end1 nil) (end2 nil))
  "Copy bytes from source array to dest array."
  (replace dest source :start1 start2 :start2 start1 :end1 end2 :end2 end1))

(defun encode-uint16-be (value &optional (array nil) (offset 0))
  "Encode a 16-bit unsigned integer in big-endian format.
   If array is provided, write to array at offset. Otherwise, return new 2-byte array."
  (declare (type (unsigned-byte 16) value)
           (type (or null (simple-array (unsigned-byte 8) (*))) array)
           (type fixnum offset))
  (let ((arr (or array (make-byte-array 2))))
    (setf (aref arr (+ offset 0)) (ldb (byte 8 8) value))
    (setf (aref arr (+ offset 1)) (ldb (byte 8 0) value))
    arr))

(defun encode-uint24-be (value &optional (array nil) (offset 0))
  "Encode a 24-bit unsigned integer in big-endian format.
   If array is provided, write to array at offset. Otherwise, return new 3-byte array."
  (declare (type (unsigned-byte 24) value)
           (type (or null (simple-array (unsigned-byte 8) (*))) array)
           (type fixnum offset))
  (let ((arr (or array (make-byte-array 3))))
    (setf (aref arr (+ offset 0)) (ldb (byte 8 16) value))
    (setf (aref arr (+ offset 1)) (ldb (byte 8 8) value))
    (setf (aref arr (+ offset 2)) (ldb (byte 8 0) value))
    arr))

(defun encode-uint32-be (value &optional (array nil) (offset 0))
  "Encode a 32-bit unsigned integer in big-endian format.
   If array is provided, write to array at offset. Otherwise, return new 4-byte array."
  (declare (type (unsigned-byte 32) value)
           (type (or null (simple-array (unsigned-byte 8) (*))) array)
           (type fixnum offset))
  (let ((arr (or array (make-byte-array 4))))
    (setf (aref arr (+ offset 0)) (ldb (byte 8 24) value))
    (setf (aref arr (+ offset 1)) (ldb (byte 8 16) value))
    (setf (aref arr (+ offset 2)) (ldb (byte 8 8) value))
    (setf (aref arr (+ offset 3)) (ldb (byte 8 0) value))
    arr))

(defun decode-uint16-be (array &optional (offset 0))
  "Decode a 16-bit unsigned integer from big-endian format."
  (declare (type (simple-array (unsigned-byte 8) (*)) array)
           (type fixnum offset))
  (+ (ash (aref array (+ offset 0)) 8)
     (aref array (+ offset 1))))

(defun decode-uint24-be (array &optional (offset 0))
  "Decode a 24-bit unsigned integer from big-endian format."
  (declare (type (simple-array (unsigned-byte 8) (*)) array)
           (type fixnum offset))
  (+ (ash (aref array (+ offset 0)) 16)
     (ash (aref array (+ offset 1)) 8)
     (aref array (+ offset 2))))

(defun decode-uint32-be (array &optional (offset 0))
  "Decode a 32-bit unsigned integer from big-endian format."
  (declare (type (simple-array (unsigned-byte 8) (*)) array)
           (type fixnum offset))
  (+ (ash (aref array (+ offset 0)) 24)
     (ash (aref array (+ offset 1)) 16)
     (ash (aref array (+ offset 2)) 8)
     (aref array (+ offset 3))))
