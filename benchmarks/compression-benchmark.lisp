;;;; compression-benchmark.lisp - Benchmark compression impact
;;;; Compares performance with and without compression for various message sizes

(ql:quickload :clgrpc :silent t)

(defparameter *port* (+ 55000 (random 1000)))
(defparameter *test-duration* 3.0)  ; seconds per test

(defun make-test-message (size)
  "Create a test message of specified size with compressible content."
  (let ((msg (make-array size :element-type '(unsigned-byte 8))))
    ;; Fill with semi-compressible pattern (repeating text-like data)
    (dotimes (i size)
      (setf (aref msg i) (mod (+ 65 (mod i 26)) 256)))
    msg))

(defun run-benchmark (server channel message encoding label)
  "Run a single benchmark and return requests/sec."
  (declare (ignore server))
  (let ((request-count 0)
        (start-time (get-internal-real-time)))
    (loop while (< (/ (- (get-internal-real-time) start-time)
                     internal-time-units-per-second)
                  *test-duration*)
          do (let ((response (clgrpc.client:call-unary
                              channel "test.Echo" "Echo"
                              message)))
               (when response (incf request-count))))
    (let* ((elapsed (/ (- (get-internal-real-time) start-time)
                      internal-time-units-per-second))
           (req-per-sec (/ request-count elapsed)))
      (format t "  ~20A: ~7,1F req/sec (~D requests)~%" label req-per-sec request-count)
      req-per-sec)))

(format t "~%Compression Benchmark~%")
(format t "=====================~%~%")
(format t "Testing impact of gzip compression at various message sizes.~%")
(format t "Compression threshold: ~D bytes~%~%" clgrpc.grpc:*compression-threshold*)

;; Create and start server
(let ((server (clgrpc.server:make-server :port *port*)))
  (clgrpc.server:register-handler
   (clgrpc.server:grpc-server-router server)
   "test.Echo" "Echo"
   (clgrpc.server:lambda-handler
     (values clgrpc.server::request-bytes 0 nil nil))
   :rpc-type :unary)

  (clgrpc.server:start-server server)
  (sleep 0.3)
  (format t "Server started on port ~D~%~%" *port*)

  (let ((channel (clgrpc.client:make-channel (format nil "localhost:~D" *port*) :secure nil)))
    (unwind-protect
        (progn
          ;; Test various message sizes
          (dolist (size '(100 500 1024 2048 5000 10000))
            (format t "Message size: ~D bytes~%" size)
            (let ((msg (make-test-message size)))

              ;; Test without compression
              (let ((clgrpc.grpc:*compression-threshold* most-positive-fixnum))
                (run-benchmark server channel msg nil "No compression"))

              ;; Test with compression (if above threshold)
              (when (>= size 1024)
                (let ((clgrpc.grpc:*compression-threshold* 512))
                  (run-benchmark server channel msg clgrpc.grpc:+compression-gzip+ "With gzip")))

              (format t "~%"))))
      (clgrpc.client:close-channel channel)))

  (clgrpc.server:stop-server server :timeout 2))

(format t "~%Note: Compression helps more on:~%")
(format t "  - Larger messages (>1KB)~%")
(format t "  - High-latency networks~%")
(format t "  - Compressible data (text, JSON, repeated patterns)~%")
(format t "~%For small messages or localhost, compression adds overhead.~%")

(sb-ext:exit :code 0)
