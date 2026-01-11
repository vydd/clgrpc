;;;; benchmark-cl-fixed.lisp - Fixed CL benchmark with simpler result collection
;;;;
;;;; Prerequisites: (ql:quickload :clgrpc-examples)
;;;; Usage: sbcl --load benchmark-cl-fixed.lisp

(ql:quickload '(:clgrpc-examples :cl-json) :silent t)

(in-package #:clgrpc.client)

(defparameter *benchmark-port* 50054)
(defparameter *benchmark-duration* 5)
(defparameter *results* nil)

(defstruct benchmark-result
  rpc-type
  num-clients
  total-requests
  duration
  requests-per-second
  errors)

;;; Worker Functions - Return (values requests errors)

(defun benchmark-unary-worker (duration)
  "Worker for unary RPC benchmark. Returns (values request-count error-count)."
  (let ((channel (make-channel (format nil "localhost:~D" *benchmark-port*) :secure nil))
        (request-count 0)
        (error-count 0)
        (start-time (get-universal-time)))

    (unwind-protect
         (loop while (< (- (get-universal-time) start-time) duration)
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

    (values request-count error-count)))

;;; Benchmark Runner

(defun run-benchmark-fixed (rpc-type num-clients duration)
  "Run a benchmark for a specific RPC type and client count."
  (format t "~%Running ~A benchmark with ~D client~:P for ~D seconds...~%"
          rpc-type num-clients duration)

  (let ((threads nil)
        (total-requests 0)
        (total-errors 0))

    ;; Start worker threads
    (dotimes (i num-clients)
      (push (bt:make-thread
             (lambda ()
               (benchmark-unary-worker duration))
             :name (format nil "benchmark-worker-~D" i))
            threads))

    ;; Wait for all threads and collect results
    (dolist (thread threads)
      (multiple-value-bind (requests errors)
          (bt:join-thread thread)
        (incf total-requests (or requests 0))
        (incf total-errors (or errors 0))))

    (let ((req-per-sec (if (> duration 0)
                           (/ total-requests duration)
                           0)))
      (format t "  Total requests: ~D~%" total-requests)
      (format t "  Total errors: ~D~%" total-errors)
      (format t "  Requests/sec: ~,1F~%~%" req-per-sec)

      (make-benchmark-result
       :rpc-type rpc-type
       :num-clients num-clients
       :total-requests total-requests
       :duration duration
       :requests-per-second req-per-sec
       :errors total-errors))))

;;; Main

(defun run-benchmarks-fixed ()
  "Run simplified benchmark suite (unary only)."
  (format t "~%╔════════════════════════════════════════════════════╗~%")
  (format t "║  Common Lisp gRPC Performance Benchmark (Fixed)   ║~%")
  (format t "╚════════════════════════════════════════════════════╝~%")
  (format t "~%NOTE: Requires RouteGuide server on port ~D~%" *benchmark-port*)

  (setf *results* nil)

  ;; Run benchmarks for unary with different client counts
  (dolist (num-clients '(1 10 100))
    (let ((result (run-benchmark-fixed :unary num-clients *benchmark-duration*)))
      (push result *results*)))

  ;; Print summary
  (format t "~%Results Summary:~%")
  (format t "~12A ~15A~%" "Clients" "Req/Sec")
  (dolist (result (reverse *results*))
    (format t "~12D ~15,1F~%"
            (benchmark-result-num-clients result)
            (benchmark-result-requests-per-second result))))

;; Don't auto-run
(format t "~%Benchmark loaded. To run:~%")
(format t "  1. Start RouteGuide server on port ~D~%" *benchmark-port*)
(format t "  2. Call (clgrpc.client::run-benchmarks-fixed)~%")
