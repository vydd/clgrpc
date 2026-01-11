;;;; test-graceful-shutdown.lisp - Test graceful server shutdown
;;;;
;;;; Demonstrates that graceful shutdown:
;;;; 1. Stops accepting new connections
;;;; 2. Sends GOAWAY to existing connections
;;;; 3. Waits for active requests to complete
;;;; 4. Times out and force-closes if needed

(ql:quickload :clgrpc :silent t)

(in-package :clgrpc.server)

;;; Define a slow service for testing

(defclass slow-service ()
  ())

(defmethod handle-unary ((handler slow-service) service method request-bytes context)
  "Simulates slow processing (5 seconds)"
  (declare (ignore service method request-bytes context))
  (format t "[HANDLER] Processing request (will take 5 seconds)...~%")
  (sleep 5)
  (format t "[HANDLER] Request complete!~%")
  (values (babel:string-to-octets "Slow response") 0 nil nil))

;;; Test graceful shutdown

(defun test-graceful-shutdown ()
  "Test graceful shutdown with active requests"
  (format t "~%═══════════════════════════════════════════════════════════~%")
  (format t "  Testing Graceful Shutdown~%")
  (format t "═══════════════════════════════════════════════════════════~%~%")

  ;; Create and start server
  (let ((server (make-server :port 50099)))
    (register-handler (grpc-server-router server)
                     "/test.SlowService/SlowMethod"
                     (make-instance 'slow-service)
                     :unary)

    (format t "1. Starting server on port 50099...~%")
    (start-server server)
    (sleep 1)

    ;; Simulate active requests in background threads
    (format t "~%2. Starting 3 slow requests (5 seconds each)...~%")
    (let ((request-threads
            (loop for i from 1 to 3
                  collect (bordeaux-threads:make-thread
                           (lambda ()
                             (handler-case
                                 (progn
                                   (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
                                     (format t "[CLIENT ~D] Sending request...~%" i)
                                     (clgrpc.client:call-unary channel "test.SlowService" "SlowMethod"
                                                              (babel:string-to-octets "test")
                                                              :timeout 10000)
                                     (format t "[CLIENT ~D] ✓ Request completed successfully!~%" i)
                                     (clgrpc.client:close-channel channel)))
                               (error (e)
                                 (format t "[CLIENT ~D] ✗ Error: ~A~%" i e))))
                           :name (format nil "client-~D" i)))))

      ;; Give requests time to start
      (sleep 1)

      ;; Initiate graceful shutdown with 10 second timeout
      (format t "~%3. Initiating graceful shutdown (10 second timeout)...~%")
      (format t "   Expected: All 3 requests should complete within 5 seconds~%~%")
      (stop-server server :timeout 10)

      ;; Wait for all client threads to finish
      (format t "~%4. Waiting for all client threads to finish...~%")
      (loop for thread in request-threads
            do (bordeaux-threads:join-thread thread))

      (format t "~%═══════════════════════════════════════════════════════════~%")
      (format t "  ✓ Graceful Shutdown Test Complete!~%")
      (format t "═══════════════════════════════════════════════════════════~%~%"))))

(defun test-timeout-shutdown ()
  "Test shutdown timeout with stuck requests"
  (format t "~%═══════════════════════════════════════════════════════════~%")
  (format t "  Testing Shutdown Timeout (Force Close)~%")
  (format t "═══════════════════════════════════════════════════════════~%~%")

  (let ((server (make-server :port 50098)))
    (register-handler (grpc-server-router server)
                     "/test.SlowService/SlowMethod"
                     (make-instance 'slow-service)
                     :unary)

    (format t "1. Starting server on port 50098...~%")
    (start-server server)
    (sleep 1)

    ;; Start slow request
    (format t "~%2. Starting slow request (5 seconds)...~%")
    (bordeaux-threads:make-thread
     (lambda ()
       (handler-case
           (let ((channel (clgrpc.client:make-channel "localhost:50098" :secure nil)))
             (format t "[CLIENT] Sending request...~%")
             (clgrpc.client:call-unary channel "test.SlowService" "SlowMethod"
                                      (babel:string-to-octets "test")
                                      :timeout 10000)
             (format t "[CLIENT] ✓ Request completed~%")
             (clgrpc.client:close-channel channel))
         (error (e)
           (format t "[CLIENT] ✗ Connection closed during request: ~A~%" e))))
     :name "client-timeout-test")

    (sleep 1)

    ;; Shutdown with SHORT timeout (will force-close)
    (format t "~%3. Initiating shutdown with 2 second timeout...~%")
    (format t "   Expected: Force-close after timeout (request won't complete)~%~%")
    (stop-server server :timeout 2)

    (format t "~%═══════════════════════════════════════════════════════════~%")
    (format t "  ✓ Timeout Test Complete!~%")
    (format t "═══════════════════════════════════════════════════════════~%~%")))

;;; Run tests

(defun run-all-tests ()
  (test-graceful-shutdown)
  (sleep 2)
  (test-timeout-shutdown)
  (format t "~%All tests complete!~%"))

;; Run if loaded directly
(run-all-tests)
