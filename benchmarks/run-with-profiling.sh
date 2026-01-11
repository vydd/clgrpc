#!/bin/bash
# Simple wrapper to run benchmark with profiling

sbcl --noinform --eval '
(progn
  (require (quote sb-sprof))
  (load "benchmark-cl.lisp")

  ;; Override to run just one quick benchmark with profiling
  (in-package :clgrpc.client)

  (defun run-profiled-test ()
    (format t "~%Starting profiled run (10 clients, 5 seconds)...~%~%")

    ;; Reset profiler
    (sb-sprof:reset)

    ;; Start profiling
    (sb-sprof:start-profiling :max-samples 100000
                              :mode :cpu
                              :sample-interval 0.001)

    ;; Run one benchmark
    (let ((result (run-benchmark :rpc-type "unary" :num-clients 10 :duration 5)))

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

  (run-profiled-test))
' 2>&1
