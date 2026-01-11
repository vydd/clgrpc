;;;; profile-benchmark.lisp - Profile the gRPC benchmark to identify bottlenecks
;;;;
;;;; Prerequisites: (ql:quickload :clgrpc-examples)
;;;; Note: Requires RouteGuide server on port 50054
;;;; Usage: sbcl --load profile-benchmark.lisp

;; Load the statistical profiler
(require 'sb-sprof)

(ql:quickload :clgrpc-examples :silent t)

;; Import packages
(use-package :clgrpc.client)

(defun run-simple-benchmark (num-clients duration)
  "Simple benchmark that creates workers directly"
  (let ((threads '())
        (request-count 0)
        (error-count 0)
        (lock (bt:make-lock)))

    ;; Create worker function
    (labels ((worker ()
               (let ((channel (make-channel "localhost:50054" :secure nil))
                     (local-requests 0)
                     (local-errors 0)
                     (start-time (get-universal-time)))
                 (unwind-protect
                      (loop while (< (- (get-universal-time) start-time) duration)
                            do (handler-case
                                   (let* ((point (routeguide:make-point :latitude 407838351
                                                                       :longitude -746143763))
                                          (request-bytes (clgrpc.grpc:proto-serialize point)))
                                     (multiple-value-bind (response-bytes status status-message)
                                         (call-unary channel "routeguide.RouteGuide" "GetFeature"
                                                    request-bytes :timeout 5000)
                                       (declare (ignore response-bytes status-message))
                                       (if (and status (= status clgrpc.grpc:+grpc-status-ok+))
                                           (incf local-requests)
                                           (incf local-errors))))
                                 (error (e)
                                   (format *error-output* "Worker error: ~A~%" e)
                                   (force-output *error-output*)
                                   (incf local-errors))))
                   (close-channel channel))
                 ;; Update shared counters
                 (bt:with-lock-held (lock)
                   (incf request-count local-requests)
                   (incf error-count local-errors)))))

      ;; Spawn workers
      (dotimes (i num-clients)
        (push (bt:make-thread #'worker :name (format nil "Worker-~D" i)) threads))

      ;; Wait for all threads
      (mapc #'bt:join-thread threads)

      ;; Return results
      (values request-count error-count))))

(defun run-profiled-benchmark ()
  "Run benchmark with SBCL statistical profiler enabled.
Note: Requires RouteGuide server already running on port 50054."
  (format t "~%Starting profiled benchmark...~%")
  (format t "NOTE: Server must be running on port 50054~%")
  (format t "Running 10 clients for 5 seconds with profiling enabled~%~%")

  ;; Reset the profiler
  (sb-sprof:reset)

  ;; Start profiling - use :cpu mode for CPU time
  (sb-sprof:start-profiling :max-samples 50000
                            :mode :cpu)

  ;; Run benchmark
  (multiple-value-bind (requests errors)
      (run-simple-benchmark 10 5)

    ;; Stop profiling
    (sb-sprof:stop-profiling)

    ;; Print results
    (format t "~%~%=== Benchmark Results ===~%")
    (format t "Total requests: ~D~%" requests)
    (format t "Errors: ~D~%" errors)
    (format t "Requests/second: ~,1F~%~%" (/ requests 5.0))

    ;; Print profiling report
    (format t "~%~%=== PROFILING REPORT (Flat - Top 50 functions) ===~%~%")
    (sb-sprof:report :type :flat :max 50)

    (format t "~%~%=== CALL GRAPH (Top 30 functions) ===~%~%")
    (sb-sprof:report :type :graph :max 30)))

;; Don't auto-run - user needs to start server first
(format t "~%Profiler loaded. To run:~%")
(format t "  1. Start RouteGuide server on port 50054~%")
(format t "  2. Call (run-profiled-benchmark)~%")
