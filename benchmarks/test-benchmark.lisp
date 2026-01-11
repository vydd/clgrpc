;;;; test-benchmark.lisp - Quick test of benchmark infrastructure
;;;;
;;;; Runs a short benchmark to verify everything works
;;;;
;;;; Prerequisites: (ql:quickload :clgrpc-examples)
;;;; Note: Requires RouteGuide server on port 50056
;;;; Usage: sbcl --load test-benchmark.lisp

(ql:quickload '(:clgrpc-examples :cl-json) :silent t)

(in-package #:clgrpc.client)

(defparameter *test-port* 50056)
(defparameter *test-duration* 3) ; Just 3 seconds for testing

(defun test-benchmark ()
  "Quick test of benchmark infrastructure."
  (format t "~%╔════════════════════════════════════════════════════╗~%")
  (format t "║  Benchmark Infrastructure Test                     ║~%")
  (format t "╚════════════════════════════════════════════════════╝~%")
  (format t "~%NOTE: Requires RouteGuide server on port ~D~%" *test-port*)

  ;; Quick unary benchmark test
  (format t "~%Running quick unary test...~%")

  (let ((channel (make-channel (format nil "localhost:~D" *test-port*) :secure nil))
        (request-count 0)
        (error-count 0)
        (start-time (get-universal-time)))

    (unwind-protect
         (loop while (< (- (get-universal-time) start-time) *test-duration*)
               do (handler-case
                      (let* ((point (routeguide:make-point :latitude 409146138 :longitude -746188906))
                             (request-bytes (clgrpc.grpc:proto-serialize point)))
                        (multiple-value-bind (response-bytes status status-message)
                            (call-unary channel "routeguide.RouteGuide" "GetFeature"
                                       request-bytes :timeout 5000)
                          (declare (ignore response-bytes status-message))
                          (if (and status (= status clgrpc.grpc:+grpc-status-ok+))
                              (incf request-count)
                              (incf error-count))))
                    (error (e)
                      (declare (ignore e))
                      (incf error-count))))
      (close-channel channel))

    (let ((req-per-sec (/ request-count *test-duration*)))
      (format t "~%Results:~%")
      (format t "  Requests: ~D~%" request-count)
      (format t "  Errors: ~D~%" error-count)
      (format t "  Requests/sec: ~,1F~%" req-per-sec)

      (if (> request-count 0)
          (format t "~%✓ Benchmark infrastructure working!~%")
          (format t "~%✗ No successful requests - check server~%")))))

;; Don't auto-run
(format t "~%Test loaded. To run:~%")
(format t "  1. Start RouteGuide server on port ~D~%" *test-port*)
(format t "  2. Call (clgrpc.client::test-benchmark)~%")
