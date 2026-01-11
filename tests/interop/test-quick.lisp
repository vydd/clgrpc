;;;; Quick test with timeout
;;;;
;;;; Prerequisites: (ql:quickload :clgrpc)
;;;; Note: Requires a gRPC server running on localhost:50051
;;;; Usage: sbcl --load test-quick.lisp

(ql:quickload :clgrpc :silent t)

(format t "~%Testing gRPC client...~%")

(let ((channel nil))
  (handler-case
      (progn
        (format t "1. Creating channel...~%")
        (setf channel (clgrpc:make-channel "localhost:50051" :secure nil))
        (format t "2. Channel created~%")

        (sleep 1)  ; Give time for connection

        (format t "3. Encoding request...~%")
        (let ((request-bytes (clgrpc.grpc:encode-hello-request "test")))
          (format t "4. Request: ~D bytes~%"  (length request-bytes))

          (format t "5. Calling with 2s timeout...~%")
          (let ((response (clgrpc:call-unary channel
                                             "helloworld.Greeter"
                                             "SayHello"
                                             request-bytes
                                             :timeout 2000)))  ; 2 second timeout
            (format t "6. Got response: ~D bytes~%" (length response))
            (let ((message (clgrpc.grpc:decode-hello-reply response)))
              (format t "7. SUCCESS: ~A~%" message)))))
    (error (e)
      (format t "ERROR: ~A~%" e)))

  (when channel
    (format t "8. Closing channel...~%")
    (clgrpc:close-channel channel))

  (format t "~%Done.~%"))
