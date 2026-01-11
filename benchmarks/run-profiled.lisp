;;;; run-profiled.lisp - Run benchmark with profiling enabled

;; Load profiler first
(require 'sb-sprof)

;; Load the benchmark
(load (merge-pathnames "benchmark-cl.lisp" *load-truename*))

;; Now we can use sb-sprof package
(in-package :clgrpc.client)

(defun run-profiled-test ()
  "Run a single benchmark with profiling"
  (format t "~%Starting profiled run (10 clients, 5 seconds)...~%~%")

  ;; Reset profiler
  (sb-sprof:reset)

  ;; Start profiling
  (sb-sprof:start-profiling :max-samples 100000
                            :mode :cpu
                            :sample-interval 0.001)

  ;; Run one benchmark
  (let ((result (run-benchmark :unary 10 5)))

    ;; Stop profiling
    (sb-sprof:stop-profiling)

    ;; Show results
    (format t "~%~%=== Results ===~%")
    (format t "Requests/sec: ~,1F~%" (getf result :requests-per-second))
    (format t "Total: ~D requests, ~D errors~%~%"
            (getf result :total-requests)
            (getf result :errors))

    ;; Show profiling data
    (format t "~%~%=== CPU PROFILING (Top 50 functions) ===~%~%")
    (sb-sprof:report :type :flat :max 50))

  (sb-ext:exit))

;; Run it
(run-profiled-test)
