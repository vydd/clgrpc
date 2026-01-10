;;;; proto-clos.lisp - CLOS-based protobuf message system
;;;;
;;;; Provides a metaclass for defining protobuf messages as CLOS classes
;;;; with automatic serialization/deserialization

(in-package #:clgrpc.grpc)

;; Ensure closer-mop is available at compile time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :closer-mop))

;;; Proto Slot Definition

(defclass proto-slot-definition (closer-mop:standard-slot-definition)
  ((field-number :initarg :field
                 :initform nil
                 :accessor proto-slot-field-number
                 :documentation "Protobuf field number (1-based)")
   (proto-type :initarg :proto-type
               :initform :int32
               :accessor proto-slot-proto-type
               :documentation "Protobuf type (:int32, :string, :message, etc.)")
   (repeated :initarg :repeated
             :initform nil
             :accessor proto-slot-repeated-p
             :documentation "Whether this is a repeated field")
   (message-type :initarg :message-type
                 :initform nil
                 :accessor proto-slot-message-type
                 :documentation "For :message type, the class name"))
  (:documentation "Slot definition for protobuf fields"))

(defclass proto-direct-slot-definition (proto-slot-definition
                                         closer-mop:standard-direct-slot-definition)
  ())

(defclass proto-effective-slot-definition (proto-slot-definition
                                            closer-mop:standard-effective-slot-definition)
  ())

;;; Proto Metaclass

(defclass proto-metaclass (closer-mop:standard-class)
  ()
  (:documentation "Metaclass for protobuf message classes"))

;; Allow proto-metaclass to be a metaclass
(defmethod closer-mop:validate-superclass ((class proto-metaclass)
                                           (superclass closer-mop:standard-class))
  t)

;; Use our custom slot definition classes
(defmethod closer-mop:direct-slot-definition-class ((class proto-metaclass) &rest initargs)
  (declare (ignore initargs))
  (find-class 'proto-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class proto-metaclass) &rest initargs)
  (declare (ignore initargs))
  (find-class 'proto-effective-slot-definition))

;; Compute effective slot definition from direct slots
(defmethod closer-mop:compute-effective-slot-definition ((class proto-metaclass)
                                                         name
                                                         direct-slots)
  (let ((effective-slot (call-next-method)))
    ;; Copy proto-specific properties from first direct slot that has them
    (loop for direct-slot in direct-slots
          when (typep direct-slot 'proto-direct-slot-definition)
          do (progn
               (when (proto-slot-field-number direct-slot)
                 (setf (proto-slot-field-number effective-slot)
                       (proto-slot-field-number direct-slot)))
               (setf (proto-slot-proto-type effective-slot)
                     (proto-slot-proto-type direct-slot))
               (setf (proto-slot-repeated-p effective-slot)
                     (proto-slot-repeated-p direct-slot))
               (when (proto-slot-message-type direct-slot)
                 (setf (proto-slot-message-type effective-slot)
                       (proto-slot-message-type direct-slot)))
               (return)))
    effective-slot))

;;; Base Class for Proto Messages

(defclass proto-message ()
  ()
  (:metaclass proto-metaclass)
  (:documentation "Base class for all protobuf messages"))

;;; Serialization

(defun get-proto-slots (class)
  "Get all proto slots for a class (slots with field numbers)."
  (remove-if-not (lambda (slot)
                   (and (typep slot 'proto-effective-slot-definition)
                        (proto-slot-field-number slot)))
                 (closer-mop:class-slots class)))

(defun proto3-default-value-p (proto-type value)
  "Check if value is the default for this proto3 type."
  (case proto-type
    ((:int32 :int64 :uint32 :uint64 :sint32 :sint64
      :fixed32 :fixed64 :sfixed32 :sfixed64)
     (eql value 0))
    (:bool
     (not value))
    ((:float :double)
     (and (numberp value) (zerop value)))
    (:string
     (or (null value) (string= value "")))
    (:bytes
     (or (null value) (zerop (length value))))
    (:message
     (null value))
    (t nil)))

(defun encode-proto-field (field-number proto-type value repeated-p message-type)
  "Encode a single field value to bytes."
  (cond
    ;; Repeated field
    (repeated-p
     (if (null value)
         nil
         (apply #'concatenate 'vector
                (mapcar (lambda (item)
                          (encode-proto-field field-number proto-type item nil message-type))
                        value))))

    ;; Proto3 default value - don't encode
    ((proto3-default-value-p proto-type value)
     nil)

    ;; Scalar types
    (t
     (ecase proto-type
       (:int32 (encode-int32-field field-number value))
       (:int64 (encode-int64-field field-number value))
       (:uint32 (encode-uint32-field field-number value))
       (:uint64 (encode-uint64-field field-number value))
       (:sint32 (encode-sint32-field field-number value))
       (:sint64 (encode-sint64-field field-number value))
       (:bool (encode-bool-field field-number value))
       (:fixed32 (encode-fixed32-field field-number value))
       (:fixed64 (encode-fixed64-field field-number value))
       (:sfixed32 (encode-sfixed32-field field-number value))
       (:sfixed64 (encode-sfixed64-field field-number value))
       (:float (encode-float-field field-number value))
       (:double (encode-double-field field-number value))
       (:string (encode-string-field field-number value))
       (:bytes (encode-bytes-field field-number value))
       (:message
        (let ((msg-bytes (proto-serialize value)))
          (encode-bytes-field field-number msg-bytes)))))))

(defgeneric proto-serialize (message)
  (:documentation "Serialize a proto-message instance to bytes."))

(defmethod proto-serialize ((message proto-message))
  "Serialize a proto-message instance to bytes."
  (let ((class (class-of message))
        (parts nil))

    ;; Collect encoded fields
    (dolist (slot (get-proto-slots class))
      (let* ((slot-name (closer-mop:slot-definition-name slot))
             (value (when (slot-boundp message slot-name)
                     (slot-value message slot-name)))
             (field-number (proto-slot-field-number slot))
             (proto-type (proto-slot-proto-type slot))
             (repeated-p (proto-slot-repeated-p slot))
             (message-type (proto-slot-message-type slot)))

        (let ((encoded (encode-proto-field field-number proto-type value repeated-p message-type)))
          (when encoded
            (push encoded parts)))))

    ;; Combine all parts
    (if parts
        (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse parts))
        (make-byte-array 0))))

;;; Deserialization

(defun decode-proto-field (proto-type bytes offset repeated-p message-type)
  "Decode a single field value from bytes. Returns (values value new-offset)."
  (ecase proto-type
    (:int32 (decode-int32 bytes offset))
    (:int64 (decode-int64 bytes offset))
    (:uint32 (decode-uint32 bytes offset))
    (:uint64 (decode-uint64 bytes offset))
    (:sint32 (decode-sint32 bytes offset))
    (:sint64 (decode-sint64 bytes offset))
    (:bool (decode-bool bytes offset))
    (:fixed32 (decode-fixed32 bytes offset))
    (:fixed64 (decode-fixed64 bytes offset))
    (:sfixed32 (decode-sfixed32 bytes offset))
    (:sfixed64 (decode-sfixed64 bytes offset))
    (:float (proto-decode-float bytes offset))
    (:double (proto-decode-double bytes offset))
    (:string (decode-string-field bytes offset))
    (:bytes (decode-bytes-field bytes offset))
    (:message
     (multiple-value-bind (msg-bytes new-offset)
         (decode-length-delimited bytes offset)
       (values (proto-deserialize message-type msg-bytes) new-offset)))))

(defgeneric proto-deserialize (class-or-name bytes)
  (:documentation "Deserialize bytes to a proto-message instance."))

(defmethod proto-deserialize ((class-name symbol) bytes)
  "Deserialize bytes to an instance of the named class."
  (proto-deserialize (find-class class-name) bytes))

(defmethod proto-deserialize ((class proto-metaclass) bytes)
  "Deserialize bytes to an instance of the given class."
  (let ((instance (allocate-instance class))
        (slot-values (make-hash-table))
        (offset 0))

    ;; Create lookup table: field-number → slot
    (let ((field-map (make-hash-table)))
      (dolist (slot (get-proto-slots class))
        (setf (gethash (proto-slot-field-number slot) field-map) slot))

      ;; Parse bytes
      (loop while (< offset (length bytes))
            do (multiple-value-bind (field-number wire-type new-offset)
                   (decode-field-tag bytes offset)
                 (setf offset new-offset)

                 (let ((slot (gethash field-number field-map)))
                   (if slot
                       (let* ((slot-name (closer-mop:slot-definition-name slot))
                              (proto-type (proto-slot-proto-type slot))
                              (repeated-p (proto-slot-repeated-p slot))
                              (message-type (proto-slot-message-type slot)))

                         (multiple-value-bind (value new-offset)
                             (decode-proto-field proto-type bytes offset repeated-p message-type)
                           (setf offset new-offset)

                           ;; Handle repeated fields
                           (if repeated-p
                               (push value (gethash slot-name slot-values))
                               (setf (gethash slot-name slot-values) value))))

                       ;; Unknown field - skip it
                       (setf offset (skip-field bytes offset wire-type))))))

      ;; Initialize instance
      (initialize-instance instance)

      ;; Set slot values
      (maphash (lambda (slot-name value)
                 (setf (slot-value instance slot-name)
                       (if (listp value)
                           (nreverse value)  ; Repeated fields accumulated in reverse
                           value)))
               slot-values)

      instance)))

;;; Convenience function for encoding wrapped in gRPC message framing

(defun proto-serialize-grpc (message)
  "Serialize a proto-message and wrap in gRPC message framing."
  (encode-grpc-message (proto-serialize message)))

(defun proto-deserialize-grpc (class-or-name grpc-bytes)
  "Unwrap gRPC message framing and deserialize to proto-message."
  (multiple-value-bind (message-bytes compressed total-read)
      (decode-grpc-message grpc-bytes)
    (declare (ignore compressed total-read))
    (proto-deserialize class-or-name message-bytes)))
