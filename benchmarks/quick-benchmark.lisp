;;;; quick-benchmark.lisp - Self-contained quick benchmark
;;;; Usage: sbcl --non-interactive --load quick-benchmark.lisp

(ql:quickload :clgrpc :silent t)

(defparameter *port* (+ 55000 (random 1000)))
(defparameter *test-duration* 5.0)  ; seconds

(format t "~%Quick Benchmark~%")
(format t "===============~%")

;; Create and start a simple echo server
(let ((server (clgrpc.server:make-server :port *port*)))
  (clgrpc.server:register-handler
   (clgrpc.server:grpc-server-router server)
   "test.Echo" "Echo"
   (clgrpc.server:lambda-handler
     (values clgrpc.server::request-bytes 0 nil nil))
   :rpc-type :unary)

  (clgrpc.server:start-server server)
  (sleep 0.3)

  (format t "Server started on port ~D~%" *port*)
  (format t "Running benchmark for ~,1F seconds...~%" *test-duration*)

  (let ((channel (clgrpc.client:make-channel (format nil "localhost:~D" *port*) :secure nil))
        (request-count 0)
        (start-time (get-internal-real-time)))

    (unwind-protect
        (loop while (< (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)
                      *test-duration*)
              do (let ((response (clgrpc.client:call-unary
                                  channel "test.Echo" "Echo"
                                  (babel:string-to-octets "Hello benchmark"))))
                   (when response (incf request-count))))
      (clgrpc.client:close-channel channel))

    (let* ((elapsed (/ (- (get-internal-real-time) start-time)
                      internal-time-units-per-second))
           (req-per-sec (/ request-count elapsed)))
      (format t "~%Results:~%")
      (format t "  Duration: ~,2F seconds~%" elapsed)
      (format t "  Requests: ~D~%" request-count)
      (format t "  Requests/sec: ~,1F~%" req-per-sec)
      (format t "  Avg latency: ~,3F ms~%" (/ 1000.0 req-per-sec))))

  (clgrpc.server:stop-server server :timeout 2))

(sb-ext:exit :code 0)
