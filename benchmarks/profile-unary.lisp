;;;; profile-unary.lisp - Simple profiled unary RPC test
;;;;
;;;; Prerequisites: (ql:quickload :clgrpc-examples)
;;;; Note: Requires RouteGuide server on port 50054
;;;; Usage: sbcl --load profile-unary.lisp

;; Load profiler
(require 'sb-sprof)

(ql:quickload :clgrpc-examples :silent t)

;; Server should already be running on port 50054

;; Now do the profiled test
(in-package :clgrpc.client)

(defun do-profiled-test ()
  "Run 10 clients for 10 seconds with profiling"
  (format t "~%Starting profiled test: 10 clients, 10 seconds~%~%")

  (let ((threads '())
        (results-lock (bt:make-lock))
        (results-list '()))

    ;; Reset and start profiler
    (sb-sprof:reset)
    (sb-sprof:start-profiling :max-samples 1000000
                              :mode :cpu
                              :sample-interval 0.01)

    ;; Worker function
    (flet ((worker (client-id)
             (let ((channel (make-channel "localhost:50054" :secure nil))
                   (request-count 0)
                   (error-count 0)
                   (start-time (get-universal-time)))
               (unwind-protect
                    (loop while (< (- (get-universal-time) start-time) 10)
                          do (handler-case
                                 (let* ((point (routeguide:make-point :latitude 409146138
                                                                     :longitude -746188906))
                                        (request-bytes (clgrpc.grpc:proto-serialize point)))
                                   (multiple-value-bind (response-bytes status status-message)
                                       (call-unary channel "routeguide.RouteGuide" "GetFeature"
                                                  request-bytes :timeout 5000)
                                     (declare (ignore response-bytes status-message))
                                     (if (and status (= status clgrpc.grpc:+grpc-status-ok+))
                                         (incf request-count)
                                         (incf error-count))))
                               (error (e)
                                 (format *error-output* "Worker ~D error: ~A~%" client-id e)
                                 (incf error-count))))
                 (close-channel channel))
               ;; Store results
               (bt:with-lock-held (results-lock)
                 (push (list :client-id client-id
                            :requests request-count
                            :errors error-count)
                       results-list)))))

      ;; Start workers
      (dotimes (i 10)
        (let ((id i))
          (push (bt:make-thread
                 (lambda () (worker id))
                 :name (format nil "Worker-~D" i))
                threads)))

      ;; Wait for completion
      (mapc #'bt:join-thread threads)

      ;; Stop profiler
      (sb-sprof:stop-profiling)

      ;; Calculate results
      (let ((total-requests 0)
            (total-errors 0))
        (dolist (result results-list)
          (incf total-requests (getf result :requests))
          (incf total-errors (getf result :errors)))

        (format t "~%=== Results ===~%")
        (format t "Total requests: ~D~%" total-requests)
        (format t "Total errors: ~D~%" total-errors)
        (format t "Requests/sec: ~,1F~%~%" (/ total-requests 10.0))

        ;; Show profiling report
        (format t "~%=== CPU PROFILING (Top 50 functions) ===~%~%")
        (sb-sprof:report :type :flat :max 50)))))

;; Don't auto-run - user needs to start server first
(format t "~%Profiler loaded. To run:~%")
(format t "  1. Start RouteGuide server on port 50054~%")
(format t "  2. Call (clgrpc.client::do-profiled-test)~%")
