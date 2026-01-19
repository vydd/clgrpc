;;;; compression.lisp - gRPC message compression support
;;;
;;; Supports gzip compression as specified in the gRPC protocol.
;;; Uses salza2 for compression and chipz for decompression.
;;;
;;; Compression is controlled by:
;;; - grpc-encoding request header (client -> server)
;;; - grpc-accept-encoding header (advertise supported encodings)
;;; - grpc-encoding response header (server -> client)

(in-package #:clgrpc.grpc)

;;; Compression Constants
;;; Using alexandria:define-constant for string constants to avoid SBCL issues

(alexandria:define-constant +compression-none+ "identity"
  :test #'equal
  :documentation "No compression")

(alexandria:define-constant +compression-gzip+ "gzip"
  :test #'equal
  :documentation "GZIP compression")

(alexandria:define-constant +compression-deflate+ "deflate"
  :test #'equal
  :documentation "DEFLATE compression (not commonly used in gRPC)")

(defparameter *supported-encodings* (list +compression-gzip+ +compression-none+)
  "List of supported compression encodings")

(defparameter *default-compression* nil
  "Default compression to use for outgoing messages. NIL means no compression.")

(defparameter *compression-threshold* 1024
  "Minimum message size in bytes to apply compression.
   Messages smaller than this are sent uncompressed.")

;;; Compression Functions

(defun gzip-compress (data)
  "Compress byte array using gzip.

   Args:
     data: Byte array to compress

   Returns:
     Compressed byte array"
  (when (zerop (length data))
    (return-from gzip-compress data))

  (let ((output (make-array (+ (length data) 64)
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    (salza2:with-compressor (compressor 'salza2:gzip-compressor
                             :callback (lambda (buffer end)
                                         (loop for i below end
                                               do (vector-push-extend (aref buffer i) output))))
      (salza2:compress-octet-vector data compressor))
    ;; Return a simple array
    (let ((result (make-array (length output) :element-type '(unsigned-byte 8))))
      (replace result output)
      result)))

(defun gzip-decompress (data)
  "Decompress gzip-compressed byte array.

   Args:
     data: Compressed byte array

   Returns:
     Decompressed byte array"
  (when (zerop (length data))
    (return-from gzip-decompress data))

  (chipz:decompress nil 'chipz:gzip data))

(defun deflate-compress (data)
  "Compress byte array using deflate.

   Args:
     data: Byte array to compress

   Returns:
     Compressed byte array"
  (when (zerop (length data))
    (return-from deflate-compress data))

  (let ((output (make-array (+ (length data) 64)
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    (salza2:with-compressor (compressor 'salza2:deflate-compressor
                             :callback (lambda (buffer end)
                                         (loop for i below end
                                               do (vector-push-extend (aref buffer i) output))))
      (salza2:compress-octet-vector data compressor))
    (let ((result (make-array (length output) :element-type '(unsigned-byte 8))))
      (replace result output)
      result)))

(defun deflate-decompress (data)
  "Decompress deflate-compressed byte array.

   Args:
     data: Compressed byte array

   Returns:
     Decompressed byte array"
  (when (zerop (length data))
    (return-from deflate-decompress data))

  (chipz:decompress nil 'chipz:deflate data))

;;; High-level compression API

(defun compress-message (data encoding)
  "Compress message using specified encoding.

   Args:
     data: Byte array to compress
     encoding: Compression encoding string (\"gzip\", \"identity\", etc.)

   Returns:
     (values compressed-data actually-compressed-p)

   If encoding is nil or \"identity\", returns original data."
  (cond
    ((or (null encoding)
         (string-equal encoding +compression-none+))
     (values data nil))

    ((string-equal encoding +compression-gzip+)
     (values (gzip-compress data) t))

    ((string-equal encoding +compression-deflate+)
     (values (deflate-compress data) t))

    (t
     ;; Unknown encoding - signal error
     (signal-grpc-unimplemented
      (format nil "Unsupported compression encoding: ~A" encoding)))))

(defun decompress-message (data encoding)
  "Decompress message using specified encoding.

   Args:
     data: Compressed byte array
     encoding: Compression encoding string (\"gzip\", \"identity\", etc.)

   Returns:
     Decompressed byte array

   Signals grpc-error if decompression fails."
  (cond
    ((or (null encoding)
         (string-equal encoding +compression-none+))
     data)

    ((string-equal encoding +compression-gzip+)
     (handler-case
         (gzip-decompress data)
       (error (e)
         (signal-grpc-internal
          (format nil "Failed to decompress gzip message: ~A" e)))))

    ((string-equal encoding +compression-deflate+)
     (handler-case
         (deflate-decompress data)
       (error (e)
         (signal-grpc-internal
          (format nil "Failed to decompress deflate message: ~A" e)))))

    (t
     (signal-grpc-unimplemented
      (format nil "Unsupported compression encoding: ~A" encoding)))))

(defun should-compress-p (data encoding)
  "Determine if a message should be compressed.

   Returns T if:
   - encoding is specified and not \"identity\"
   - data size exceeds *compression-threshold*"
  (and encoding
       (not (string-equal encoding +compression-none+))
       (> (length data) *compression-threshold*)))

(defun supported-encoding-p (encoding)
  "Check if an encoding is supported."
  (member encoding *supported-encodings* :test #'string-equal))

(defun format-accept-encoding ()
  "Format grpc-accept-encoding header value."
  (format nil "~{~A~^, ~}" *supported-encodings*))
