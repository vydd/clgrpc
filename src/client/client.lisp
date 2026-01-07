;;;; client.lisp - Client API (stub)

(in-package #:clgrpc.client)

;; Stubs for Phase 1
(defun make-channel (target &key secure)
  "Stub: Create gRPC channel (to be implemented in Phase 5)"
  (declare (ignore target secure))
  nil)

(defun close-channel (channel)
  "Stub: Close gRPC channel (to be implemented in Phase 5)"
  (declare (ignore channel))
  nil)

(defun call-unary (channel service method request &key timeout metadata)
  "Stub: Make unary RPC call (to be implemented in Phase 5)"
  (declare (ignore channel service method request timeout metadata))
  nil)
