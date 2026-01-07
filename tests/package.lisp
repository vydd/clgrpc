;;;; package.lisp - Test package definitions

(defpackage #:clgrpc-tests
  (:documentation "Test suite for clgrpc")
  (:use #:cl #:fiveam #:clgrpc.utils #:clgrpc.http2 #:clgrpc.grpc #:clgrpc)
  (:export
   ;; Test suites
   #:clgrpc-all
   #:http2-tests
   #:grpc-tests
   #:client-tests
   #:server-tests
   #:interop-tests
   #:integration-tests

   ;; Individual test suites
   #:frame-tests
   #:hpack-tests
   #:stream-tests
   #:connection-tests
   #:protocol-tests
   #:metadata-tests

))

(in-package #:clgrpc-tests)

(defun bytes (&rest values)
  "Create a byte array from given values"
  (make-array (length values) :element-type '(unsigned-byte 8) :initial-contents values))

;;; Test Suite Hierarchy

(def-suite clgrpc-all
  :description "All clgrpc tests")

(def-suite http2-tests
  :in clgrpc-all
  :description "HTTP/2 implementation tests")

(def-suite grpc-tests
  :in clgrpc-all
  :description "gRPC protocol tests")

(def-suite client-tests
  :in clgrpc-all
  :description "gRPC client tests")

(def-suite server-tests
  :in clgrpc-all
  :description "gRPC server tests")

(def-suite interop-tests
  :in clgrpc-all
  :description "Interoperability tests with official gRPC implementations")

(def-suite integration-tests
  :in clgrpc-all
  :description "End-to-end integration tests")
