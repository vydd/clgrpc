;;;; test-debug.lisp - Simple debug test for gRPC channel
;;;;
;;;; Prerequisites: (ql:quickload :clgrpc)
;;;; Note: Requires server on port 50054
;;;; Usage: sbcl --load test-debug.lisp

(ql:quickload :clgrpc :silent t)

;; Enable debug mode
(setf clgrpc.http2:*debug* t)

(in-package #:clgrpc.client)

;; Simple test
(let ((channel (make-channel "localhost:50054" :secure nil)))
  (sleep 1)
  (format t "~%Channel created~%")
  (close-channel channel))
