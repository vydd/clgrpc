;;;; protobuf-simple.lisp - Complete protobuf implementation
;;;;
;;;; Full Protocol Buffers (proto3) implementation for gRPC.
;;;; Supports all basic types, repeated fields, and embedded messages.
;;;;
;;;; Wire format: https://protobuf.dev/programming-guides/encoding/

(in-package #:clgrpc.grpc)

;;; Protobuf Wire Types
(defparameter +wire-type-varint+ 0)
(defparameter +wire-type-64bit+ 1)
(defparameter +wire-type-length-delimited+ 2)
(defparameter +wire-type-start-group+ 3)
(defparameter +wire-type-end-group+ 4)
(defparameter +wire-type-32bit+ 5)

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

;;; Zigzag Encoding (for signed integers)

(defun encode-zigzag-32 (value)
  "Encode signed 32-bit integer using zigzag encoding.
   Maps: 0 → 0, -1 → 1, 1 → 2, -2 → 3, 2 → 4, ..."
  (logxor (ash value 1) (ash value -31)))

(defun decode-zigzag-32 (value)
  "Decode zigzag-encoded signed 32-bit integer."
  (logxor (ash value -1) (- (logand value 1))))

(defun encode-zigzag-64 (value)
  "Encode signed 64-bit integer using zigzag encoding."
  (logxor (ash value 1) (ash value -63)))

(defun decode-zigzag-64 (value)
  "Decode zigzag-encoded signed 64-bit integer."
  (logxor (ash value -1) (- (logand value 1))))

;;; Fixed-Width Integer Encoding

(defun encode-fixed32 (value)
  "Encode 32-bit fixed-width integer (little-endian). Returns 4-byte array."
  (let ((result (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref result 0) (logand value #xFF))
    (setf (aref result 1) (logand (ash value -8) #xFF))
    (setf (aref result 2) (logand (ash value -16) #xFF))
    (setf (aref result 3) (logand (ash value -24) #xFF))
    result))

(defun decode-fixed32 (bytes offset)
  "Decode 32-bit fixed-width integer. Returns (values value new-offset)."
  (let ((value (logior (aref bytes offset)
                      (ash (aref bytes (+ offset 1)) 8)
                      (ash (aref bytes (+ offset 2)) 16)
                      (ash (aref bytes (+ offset 3)) 24))))
    (values value (+ offset 4))))

(defun encode-fixed64 (value)
  "Encode 64-bit fixed-width integer (little-endian). Returns 8-byte array."
  (let ((result (make-array 8 :element-type '(unsigned-byte 8)))
        (unsigned-value (logand value #xFFFFFFFFFFFFFFFF)))
    (loop for i from 0 to 7
          do (setf (aref result i) (logand (ash unsigned-value (* i -8)) #xFF)))
    result))

(defun decode-fixed64 (bytes offset)
  "Decode 64-bit fixed-width integer. Returns (values value new-offset)."
  (let ((value 0))
    (loop for i from 0 to 7
          do (setf value (logior value (ash (aref bytes (+ offset i)) (* i 8)))))
    (values value (+ offset 8))))

;;; Floating Point Encoding (IEEE 754)


(defun pb-encode-float (value)
  "Encode 32-bit float (IEEE 754). Returns 4-byte array."
  ;; Use SBCL's internal float representation
  #+sbcl
  (let ((bits (logand #xFFFFFFFF (sb-kernel:single-float-bits value))))
    (encode-fixed32 bits))
  #-sbcl
  (error "Float encoding not implemented for this Lisp"))

(defun pb-decode-float (bytes offset)
  "Decode 32-bit float (IEEE 754). Returns (values value new-offset)."
  (multiple-value-bind (bits new-offset)
      (decode-fixed32 bytes offset)
    #+sbcl
    (let ((signed-bits (if (>= bits #x80000000)
                          (- bits #x100000000)
                          bits)))
      (values (sb-kernel:make-single-float signed-bits) new-offset))
    #-sbcl
    (error "Float decoding not implemented for this Lisp")))

(defun pb-encode-double (value)
  "Encode 64-bit double (IEEE 754). Returns 8-byte array."
  #+sbcl
  (multiple-value-bind (hi lo)
      (sb-kernel:double-float-bits value)
    (let ((bits (if lo
                   ;; Two values returned (hi, lo) - combine them
                   (logior (ash (logand hi #xFFFFFFFF) 32)
                          (logand lo #xFFFFFFFF))
                   ;; Single value returned - use it directly
                   hi)))
      (encode-fixed64 bits)))
  #-sbcl
  (error "Double encoding not implemented for this Lisp"))

(defun pb-decode-double (bytes offset)
  "Decode 64-bit double (IEEE 754). Returns (values value new-offset)."
  (multiple-value-bind (bits new-offset)
      (decode-fixed64 bytes offset)
    #+sbcl
    ;; Reinterpret 64-bit integer as double using direct memory access
    (let ((bits-array (make-array 8 :element-type '(unsigned-byte 8))))
      (loop for i from 0 to 7
            do (setf (aref bits-array i) (logand (ash bits (* i -8)) #xFF)))
      (sb-sys:with-pinned-objects (bits-array)
        (let ((sap (sb-sys:vector-sap bits-array)))
          (values (sb-sys:sap-ref-double sap 0) new-offset))))
    #-sbcl
    (error "Double decoding not implemented for this Lisp")))

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

(defun encode-bytes-field (field-number bytes-value)
  "Encode a bytes field. Returns byte array."
  (let* ((tag (encode-field-tag field-number +wire-type-length-delimited+))
         (length (encode-varint (length bytes-value)))
         (result (make-array (+ (length tag) (length length) (length bytes-value))
                            :element-type '(unsigned-byte 8))))
    (replace result tag)
    (replace result length :start1 (length tag))
    (replace result bytes-value :start1 (+ (length tag) (length length)))
    result))

(defun decode-bytes-field (bytes offset)
  "Decode a length-delimited bytes field. Returns (values bytes-array new-offset)."
  (multiple-value-bind (length new-offset)
      (decode-varint bytes offset)
    (let ((bytes-value (subseq bytes new-offset (+ new-offset length))))
      (values bytes-value (+ new-offset length)))))

;;; Generic Field Encoding (all protobuf types)

(defun encode-int32-field (field-number value)
  "Encode int32 field (varint)."
  (concatenate '(vector (unsigned-byte 8))
               (encode-field-tag field-number +wire-type-varint+)
               (encode-varint (logand value #xFFFFFFFF))))

(defun encode-int64-field (field-number value)
  "Encode int64 field (varint). Negative values are treated as large unsigned values."
  (let ((unsigned-value (if (< value 0)
                           (+ value #x10000000000000000)  ; Convert to unsigned 64-bit
                           value)))
    (concatenate '(vector (unsigned-byte 8))
                 (encode-field-tag field-number +wire-type-varint+)
                 (encode-varint unsigned-value))))

(defun encode-uint32-field (field-number value)
  "Encode uint32 field (varint)."
  (encode-int32-field field-number value))

(defun encode-uint64-field (field-number value)
  "Encode uint64 field (varint)."
  (encode-int64-field field-number value))

(defun encode-sint32-field (field-number value)
  "Encode sint32 field (zigzag-encoded varint)."
  (concatenate '(vector (unsigned-byte 8))
               (encode-field-tag field-number +wire-type-varint+)
               (encode-varint (encode-zigzag-32 value))))

(defun encode-sint64-field (field-number value)
  "Encode sint64 field (zigzag-encoded varint)."
  (concatenate '(vector (unsigned-byte 8))
               (encode-field-tag field-number +wire-type-varint+)
               (encode-varint (encode-zigzag-64 value))))

(defun encode-bool-field (field-number value)
  "Encode bool field (varint 0 or 1)."
  (concatenate '(vector (unsigned-byte 8))
               (encode-field-tag field-number +wire-type-varint+)
               (encode-varint (if value 1 0))))

(defun encode-fixed32-field (field-number value)
  "Encode fixed32 field."
  (concatenate '(vector (unsigned-byte 8))
               (encode-field-tag field-number +wire-type-32bit+)
               (encode-fixed32 value)))

(defun encode-fixed64-field (field-number value)
  "Encode fixed64 field."
  (concatenate '(vector (unsigned-byte 8))
               (encode-field-tag field-number +wire-type-64bit+)
               (encode-fixed64 value)))

(defun encode-sfixed32-field (field-number value)
  "Encode sfixed32 field (signed fixed32)."
  (encode-fixed32-field field-number value))

(defun encode-sfixed64-field (field-number value)
  "Encode sfixed64 field (signed fixed64)."
  (encode-fixed64-field field-number value))

(defun encode-float-field (field-number value)
  "Encode float field."
  (concatenate '(vector (unsigned-byte 8))
               (encode-field-tag field-number +wire-type-32bit+)
               (pb-encode-float value)))

(defun encode-double-field (field-number value)
  "Encode double field."
  (concatenate '(vector (unsigned-byte 8))
               (encode-field-tag field-number +wire-type-64bit+)
               (pb-encode-double value)))

(defun encode-enum-field (field-number value)
  "Encode enum field (varint)."
  (encode-int32-field field-number value))

;;; HelloWorld Message Encoding
;;;
;;; Decode Functions (low-level API for proto-clos and advanced users)

(defun decode-int32 (bytes offset)
  "Decode int32 value (varint). Returns (values value new-offset)."
  (multiple-value-bind (value new-offset)
      (decode-varint bytes offset)
    ;; Convert to signed 32-bit
    (let ((val32 (logand value #xFFFFFFFF)))
      (values (if (> val32 #x7FFFFFFF)
                  (- val32 #x100000000)
                  val32)
              new-offset))))

(defun decode-int64 (bytes offset)
  "Decode int64 value (varint). Returns (values value new-offset)."
  (multiple-value-bind (value new-offset)
      (decode-varint bytes offset)
    ;; Convert from unsigned 64-bit to signed 64-bit
    (values (if (> value #x7FFFFFFFFFFFFFFF)
                (- value #x10000000000000000)
                value)
            new-offset)))

(defun decode-uint32 (bytes offset)
  "Decode uint32 value (varint). Returns (values value new-offset)."
  (multiple-value-bind (value new-offset)
      (decode-varint bytes offset)
    (values (logand value #xFFFFFFFF) new-offset)))

(defun decode-uint64 (bytes offset)
  "Decode uint64 value (varint). Returns (values value new-offset)."
  (decode-varint bytes offset))

(defun decode-sint32 (bytes offset)
  "Decode sint32 value (zigzag-encoded varint). Returns (values value new-offset)."
  (multiple-value-bind (value new-offset)
      (decode-varint bytes offset)
    (values (decode-zigzag-32 value) new-offset)))

(defun decode-sint64 (bytes offset)
  "Decode sint64 value (zigzag-encoded varint). Returns (values value new-offset)."
  (multiple-value-bind (value new-offset)
      (decode-varint bytes offset)
    (values (decode-zigzag-64 value) new-offset)))

(defun decode-bool (bytes offset)
  "Decode bool value (varint). Returns (values value new-offset)."
  (multiple-value-bind (value new-offset)
      (decode-varint bytes offset)
    (values (not (zerop value)) new-offset)))

(defun decode-sfixed32 (bytes offset)
  "Decode sfixed32 value (fixed 32-bit). Returns (values value new-offset)."
  (let ((val (decode-fixed32 bytes offset)))
    (values (if (> val #x7FFFFFFF)
                (- val #x100000000)
                val)
            (+ offset 4))))

(defun decode-sfixed64 (bytes offset)
  "Decode sfixed64 value (fixed 64-bit). Returns (values value new-offset)."
  (let ((val (decode-fixed64 bytes offset)))
    (values (if (> val #x7FFFFFFFFFFFFFFF)
                (- val #x10000000000000000)
                val)
            (+ offset 8))))

(defun proto-decode-float (bytes offset)
  "Decode float value (32-bit IEEE 754). Returns (values value new-offset)."
  (pb-decode-float bytes offset))

(defun proto-decode-double (bytes offset)
  "Decode double value (64-bit IEEE 754). Returns (values value new-offset)."
  (pb-decode-double bytes offset))

(defun decode-length-delimited (bytes offset)
  "Decode length-delimited value (bytes). Returns (values bytes new-offset)."
  (multiple-value-bind (length new-offset)
      (decode-varint bytes offset)
    (let ((value (subseq bytes new-offset (+ new-offset length))))
      (values value (+ new-offset length)))))
