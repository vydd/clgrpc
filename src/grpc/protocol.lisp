;;;; protocol.lisp - gRPC protocol (stub)

(in-package #:clgrpc.grpc)

;; Stubs for Phase 1
(defun encode-grpc-message (message &key compressed)
  "Stub: Encode gRPC message (to be implemented in Phase 4)"
  (declare (ignore message compressed))
  #())

(defun decode-grpc-message (bytes message-type)
  "Stub: Decode gRPC message (to be implemented in Phase 4)"
  (declare (ignore bytes message-type))
  nil)
