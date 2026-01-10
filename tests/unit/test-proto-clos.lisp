;;;; test-proto-clos.lisp - Tests for CLOS-based protobuf message system
;;;;
;;;; Tests the proto-metaclass and proto-message functionality

(in-package #:cl-user)

(defpackage #:clgrpc.tests.proto-clos
  (:use #:cl #:clgrpc.grpc #:clgrpc.utils)
  (:export #:run-tests))

(in-package #:clgrpc.tests.proto-clos)

;;; Test Helper

(defun test-case (name)
  (format t "~%Testing: ~A~%" name))

(defun assert-equal (expected actual &optional description)
  (unless (equal expected actual)
    (error "Assertion failed~@[: ~A~]~%  Expected: ~S~%  Actual:   ~S"
           description expected actual)))

(defun assert-true (condition &optional description)
  (unless condition
    (error "Assertion failed~@[: ~A~]~%  Expected: true~%  Actual:   nil"
           description)))

;;; Test Messages

;; Simple message with scalar fields
(defclass point (proto-message)
  ((latitude
    :initarg :latitude
    :initform 0
    :field 1
    :proto-type :int32
    :accessor point-latitude)
   (longitude
    :initarg :longitude
    :initform 0
    :field 2
    :proto-type :int32
    :accessor point-longitude))
  (:metaclass proto-metaclass)
  (:documentation "A point in E7 representation"))

;; Message with string field
(defclass feature (proto-message)
  ((name
    :initarg :name
    :initform ""
    :field 1
    :proto-type :string
    :accessor feature-name)
   (location
    :initarg :location
    :initform nil
    :field 2
    :proto-type :message
    :message-type point
    :accessor feature-location))
  (:metaclass proto-metaclass)
  (:documentation "A named feature at a location"))

;; Message with repeated field
(defclass route-summary (proto-message)
  ((point-count
    :initarg :point-count
    :initform 0
    :field 1
    :proto-type :int32
    :accessor route-summary-point-count)
   (feature-count
    :initarg :feature-count
    :initform 0
    :field 2
    :proto-type :int32
    :accessor route-summary-feature-count)
   (distance
    :initarg :distance
    :initform 0
    :field 3
    :proto-type :int32
    :accessor route-summary-distance)
   (elapsed-time
    :initarg :elapsed-time
    :initform 0
    :field 4
    :proto-type :int32
    :accessor route-summary-elapsed-time))
  (:metaclass proto-metaclass)
  (:documentation "Summary of a route"))

;; Message with repeated scalar field
(defclass tag-list (proto-message)
  ((tags
    :initarg :tags
    :initform nil
    :field 1
    :proto-type :string
    :repeated t
    :accessor tag-list-tags))
  (:metaclass proto-metaclass)
  (:documentation "A list of tags"))

;; Message with various types
(defclass complex-message (proto-message)
  ((int32-field
    :initarg :int32-field
    :initform 0
    :field 1
    :proto-type :int32)
   (int64-field
    :initarg :int64-field
    :initform 0
    :field 2
    :proto-type :int64)
   (uint32-field
    :initarg :uint32-field
    :initform 0
    :field 3
    :proto-type :uint32)
   (uint64-field
    :initarg :uint64-field
    :initform 0
    :field 4
    :proto-type :uint64)
   (sint32-field
    :initarg :sint32-field
    :initform 0
    :field 5
    :proto-type :sint32)
   (sint64-field
    :initarg :sint64-field
    :initform 0
    :field 6
    :proto-type :sint64)
   (bool-field
    :initarg :bool-field
    :initform nil
    :field 7
    :proto-type :bool)
   (float-field
    :initarg :float-field
    :initform 0.0
    :field 8
    :proto-type :float)
   (double-field
    :initarg :double-field
    :initform 0.0d0
    :field 9
    :proto-type :double)
   (string-field
    :initarg :string-field
    :initform ""
    :field 10
    :proto-type :string)
   (bytes-field
    :initarg :bytes-field
    :initform nil
    :field 11
    :proto-type :bytes))
  (:metaclass proto-metaclass)
  (:documentation "Message with various field types"))

;;; Tests

(defun test-simple-point ()
  (test-case "Simple Point message")

  ;; Create a point
  (let ((p1 (make-instance 'point
                          :latitude 409146138
                          :longitude -746188906)))

    ;; Check accessors
    (assert-equal 409146138 (point-latitude p1) "Latitude accessor")
    (assert-equal -746188906 (point-longitude p1) "Longitude accessor")

    ;; Serialize
    (let ((bytes (proto-serialize p1)))
      (assert-true (> (length bytes) 0) "Serialized bytes not empty")

      ;; Deserialize
      (let ((p2 (proto-deserialize 'point bytes)))
        (assert-equal (point-latitude p1) (point-latitude p2) "Latitude round-trip")
        (assert-equal (point-longitude p1) (point-longitude p2) "Longitude round-trip"))))

  (format t "  ✓ Simple Point test passed~%"))

(defun test-nested-message ()
  (test-case "Nested message (Feature with Point)")

  ;; Create a feature with location
  (let ((f1 (make-instance 'feature
                          :name "Central Park"
                          :location (make-instance 'point
                                                  :latitude 407564842
                                                  :longitude -742082278))))

    ;; Check accessors
    (assert-equal "Central Park" (feature-name f1) "Feature name")
    (assert-true (typep (feature-location f1) 'point) "Location is a point")
    (assert-equal 407564842 (point-latitude (feature-location f1)) "Location latitude")

    ;; Serialize
    (let ((bytes (proto-serialize f1)))
      (assert-true (> (length bytes) 0) "Serialized bytes not empty")

      ;; Deserialize
      (let ((f2 (proto-deserialize 'feature bytes)))
        (assert-equal (feature-name f1) (feature-name f2) "Name round-trip")
        (assert-true (typep (feature-location f2) 'point) "Deserialized location is a point")
        (assert-equal (point-latitude (feature-location f1))
                     (point-latitude (feature-location f2))
                     "Location latitude round-trip"))))

  (format t "  ✓ Nested message test passed~%"))

(defun test-repeated-field ()
  (test-case "Repeated field")

  ;; Create a tag list
  (let ((tl1 (make-instance 'tag-list
                           :tags '("important" "urgent" "todo"))))

    ;; Check accessor
    (assert-equal 3 (length (tag-list-tags tl1)) "Tag count")
    (assert-equal "important" (first (tag-list-tags tl1)) "First tag")

    ;; Serialize
    (let ((bytes (proto-serialize tl1)))
      (assert-true (> (length bytes) 0) "Serialized bytes not empty")

      ;; Deserialize
      (let ((tl2 (proto-deserialize 'tag-list bytes)))
        (assert-equal (length (tag-list-tags tl1))
                     (length (tag-list-tags tl2))
                     "Tag count round-trip")
        (assert-equal (tag-list-tags tl1)
                     (tag-list-tags tl2)
                     "Tags round-trip"))))

  (format t "  ✓ Repeated field test passed~%"))

(defun test-empty-message ()
  (test-case "Empty message (all default values)")

  ;; Create empty point
  (let ((p1 (make-instance 'point)))

    ;; Serialize
    (let ((bytes (proto-serialize p1)))
      ;; Empty message should serialize to empty bytes (proto3 semantics)
      (assert-equal 0 (length bytes) "Empty message serializes to empty bytes")

      ;; Deserialize
      (let ((p2 (proto-deserialize 'point bytes)))
        (assert-equal 0 (point-latitude p2) "Default latitude")
        (assert-equal 0 (point-longitude p2) "Default longitude"))))

  (format t "  ✓ Empty message test passed~%"))

(defun test-partial-message ()
  (test-case "Partial message (some fields unset)")

  ;; Create point with only latitude set
  (let ((p1 (make-instance 'point :latitude 12345)))

    (assert-equal 12345 (point-latitude p1) "Latitude set")
    (assert-equal 0 (point-longitude p1) "Longitude default")

    ;; Serialize
    (let ((bytes (proto-serialize p1)))
      (assert-true (> (length bytes) 0) "Serialized bytes not empty")

      ;; Deserialize
      (let ((p2 (proto-deserialize 'point bytes)))
        (assert-equal 12345 (point-latitude p2) "Latitude round-trip")
        (assert-equal 0 (point-longitude p2) "Longitude default round-trip"))))

  (format t "  ✓ Partial message test passed~%"))

(defun test-grpc-framing ()
  (test-case "gRPC message framing")

  ;; Create a point
  (let ((p1 (make-instance 'point
                          :latitude 123456
                          :longitude -654321)))

    ;; Serialize with gRPC framing
    (let ((grpc-bytes (proto-serialize-grpc p1)))
      (assert-true (>= (length grpc-bytes) 5) "gRPC message has header")

      ;; First byte is compression flag (should be 0)
      (assert-equal 0 (aref grpc-bytes 0) "Compression flag is 0")

      ;; Deserialize with gRPC framing
      (let ((p2 (proto-deserialize-grpc 'point grpc-bytes)))
        (assert-equal (point-latitude p1) (point-latitude p2) "Latitude round-trip with framing")
        (assert-equal (point-longitude p1) (point-longitude p2) "Longitude round-trip with framing"))))

  (format t "  ✓ gRPC framing test passed~%"))

(defun test-various-types ()
  (test-case "Various field types")

  ;; Create message with various types
  (let ((m1 (make-instance 'complex-message
                          :int32-field -42
                          :int64-field -9223372036854775807
                          :uint32-field 4294967295
                          :uint64-field 18446744073709551615
                          :sint32-field -100
                          :sint64-field -1000
                          :bool-field t
                          :float-field 3.14
                          :double-field 2.718281828d0
                          :string-field "Hello, World!"
                          :bytes-field #(1 2 3 4 5))))

    ;; Serialize
    (let ((bytes (proto-serialize m1)))
      (assert-true (> (length bytes) 0) "Serialized bytes not empty")

      ;; Deserialize
      (let ((m2 (proto-deserialize 'complex-message bytes)))
        (assert-equal (slot-value m1 'int32-field) (slot-value m2 'int32-field) "int32 round-trip")
        (assert-equal (slot-value m1 'int64-field) (slot-value m2 'int64-field) "int64 round-trip")
        (assert-equal (slot-value m1 'uint32-field) (slot-value m2 'uint32-field) "uint32 round-trip")
        (assert-equal (slot-value m1 'uint64-field) (slot-value m2 'uint64-field) "uint64 round-trip")
        (assert-equal (slot-value m1 'sint32-field) (slot-value m2 'sint32-field) "sint32 round-trip")
        (assert-equal (slot-value m1 'sint64-field) (slot-value m2 'sint64-field) "sint64 round-trip")
        (assert-equal (slot-value m1 'bool-field) (slot-value m2 'bool-field) "bool round-trip")
        ;; Float comparison with epsilon
        (assert-true (< (abs (- (slot-value m1 'float-field) (slot-value m2 'float-field))) 0.001)
                    "float round-trip")
        (assert-true (< (abs (- (slot-value m1 'double-field) (slot-value m2 'double-field))) 0.000001d0)
                    "double round-trip")
        (assert-equal (slot-value m1 'string-field) (slot-value m2 'string-field) "string round-trip")
        (assert-equal (coerce (slot-value m1 'bytes-field) 'list)
                     (coerce (slot-value m2 'bytes-field) 'list)
                     "bytes round-trip"))))

  (format t "  ✓ Various types test passed~%"))

(defun test-nil-message-field ()
  (test-case "Nil message field (optional nested message)")

  ;; Create feature without location
  (let ((f1 (make-instance 'feature :name "Unknown Place")))

    (assert-equal "Unknown Place" (feature-name f1) "Feature name")
    (assert-equal nil (feature-location f1) "Location is nil")

    ;; Serialize
    (let ((bytes (proto-serialize f1)))
      (assert-true (> (length bytes) 0) "Serialized bytes not empty")

      ;; Deserialize
      (let ((f2 (proto-deserialize 'feature bytes)))
        (assert-equal (feature-name f1) (feature-name f2) "Name round-trip")
        (assert-equal nil (feature-location f2) "Location is nil after round-trip"))))

  (format t "  ✓ Nil message field test passed~%"))

;;; Test Runner

(defun run-tests ()
  "Run all proto-clos tests."
  (format t "~%======================================~%")
  (format t "Proto-CLOS Test Suite~%")
  (format t "======================================~%")

  (handler-case
      (progn
        (test-simple-point)
        (test-nested-message)
        (test-repeated-field)
        (test-empty-message)
        (test-partial-message)
        (test-grpc-framing)
        (test-various-types)
        (test-nil-message-field)

        (format t "~%======================================~%")
        (format t "All tests passed! ✓~%")
        (format t "======================================~%")
        t)
    (error (e)
      (format t "~%~%FAILED: ~A~%" e)
      nil)))

;; Run tests if loaded directly
(when (member :clgrpc-test-proto-clos *features*)
  (run-tests))
