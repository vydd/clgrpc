;;;; metadata.lisp - gRPC metadata handling (stub)

(in-package #:clgrpc.grpc)

;; Stub for Phase 1
(defun encode-metadata (metadata)
  "Stub: Encode metadata (to be implemented in Phase 4)"
  (declare (ignore metadata))
  nil)

(defun decode-metadata (headers)
  "Stub: Decode metadata (to be implemented in Phase 4)"
  (declare (ignore headers))
  nil)

(defun encode-grpc-request-headers (service method &key authority timeout metadata)
  "Stub: Encode gRPC request headers (to be implemented in Phase 4)"
  (declare (ignore service method authority timeout metadata))
  nil)

(defun encode-grpc-response-headers (&key metadata)
  "Stub: Encode gRPC response headers (to be implemented in Phase 4)"
  (declare (ignore metadata))
  nil)

(defun decode-grpc-trailers (headers)
  "Stub: Decode gRPC trailers (to be implemented in Phase 4)"
  (declare (ignore headers))
  (values 0 ""))
