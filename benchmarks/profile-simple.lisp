;;;; profile-simple.lisp - Minimal profiler for existing benchmark

;; Load the statistical profiler
(require 'sb-sprof)

;; Load the working benchmark
(load (merge-pathnames "benchmark-cl.lisp" *load-truename*))

;; Override the run-all-benchmarks to just run one quick test with profiling
(in-package #:clgrpc.client)

(defun profile-single-test ()
  "Run a single benchmark test with profiling"
  (format t "~%~%Starting profiled benchmark...~%")
  (format t "Will run 10 clients for 5 seconds~%~%")

  ;; Make sure server is running (it should be started by benchmark-cl.lisp)
  (sleep 2)

  ;; Reset and start profiler
  (sb-sprof:reset)
  (sb-sprof:start-profiling :max-samples 100000
                            :mode :cpu
                            :sample-interval 0.001)

  ;; Run the benchmark
  (let* ((num-clients 10)
         (duration 5)
         (threads '())
         (results-lock (bt:make-lock))
         (results-list '())
         (stop-flag nil))

    ;; Start worker threads
    (dotimes (i num-clients)
      (push (bt:make-thread
             (lambda ()
               (benchmark-unary-worker i duration stop-flag results-lock results-list))
             :name (format nil "Worker-~D" i))
            threads))

    ;; Wait for completion
    (mapc #'bt:join-thread threads)

    ;; Stop profiling
    (sb-sprof:stop-profiling)

    ;; Calculate results
    (let ((total-requests 0)
          (total-errors 0))
      (dolist (result results-list)
        (incf total-requests (getf result :requests))
        (incf total-errors (getf result :errors)))

      ;; Print results
      (format t "~%~%=== Benchmark Results ===~%")
      (format t "Total requests: ~D~%" total-requests)
      (format t "Total errors: ~D~%" total-errors)
      (format t "Requests/second: ~,1F~%~%" (/ total-requests duration))

      ;; Print profiling report
      (format t "~%~%=== PROFILING REPORT (Top 50 functions by CPU time) ===~%~%")
      (sb-sprof:report :type :flat :max 50)

      (format t "~%~%=== CALL GRAPH ===~%~%")
      (sb-sprof:report :type :graph :max 30))))

;; Run it
(profile-single-test)

;; Exit
(sb-ext:exit :code 0)
