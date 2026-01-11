;;;; Simple test to diagnose issues
;;;;
;;;; Prerequisites: (ql:quickload :clgrpc)
;;;; Note: Requires a gRPC server running on localhost:50051
;;;; Usage: sbcl --load test-simple.lisp

(ql:quickload :clgrpc :silent t)

(format t "~%Testing gRPC client...~%")

(handler-case
    (progn
      (format t "1. Creating channel...~%")
      (let ((channel (clgrpc:make-channel "localhost:50051" :secure nil)))
        (format t "2. Channel created!~%")

        (format t "3. Encoding request...~%")
        (let ((request-bytes (clgrpc.grpc:encode-hello-request "world")))
          (format t "4. Request encoded: ~D bytes~%"  (length request-bytes))

          (format t "5. Calling RPC...~%")
          (sleep 2)  ; Give server time

          (clgrpc:close-channel channel)
          (format t "6. Channel closed~%"))))
  (error (e)
    (format t "ERROR: ~A~%" e)
    (format t "~%Backtrace:~%")
    (sb-debug:print-backtrace :stream *standard-output* :count 20)))

(format t "~%Test complete.~%")
