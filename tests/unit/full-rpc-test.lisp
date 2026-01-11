;;;; Full RPC test with Go server - simplified and instrumented
;;;;
;;;; Prerequisites:
;;;;   (ql:quickload :clgrpc)
;;;;   Go server binary must be built: cd tests/interop && ./setup.sh
;;;; Usage: sbcl --load full-rpc-test.lisp

(ql:quickload :clgrpc :silent t)

(format t "~%=== Full RPC Test (HelloWorld) ===~%")

(defvar *server-binary*
  (asdf:system-relative-pathname :clgrpc "tests/interop/bin/greeter_server"))

;; Start Go server
(format t "1. Starting Go server...~%")
(let ((server (uiop:launch-program
               (list (namestring *server-binary*))
               :output :stream :error-output :stream)))
  (sleep 1)
  (format t "   Server PID: ~A~%~%" (uiop:process-info-pid server))

  (unwind-protect
      (handler-case
          (progn
            ;; Create channel
            (format t "2. Creating channel...~%")
            (let ((channel (clgrpc:make-channel "localhost:50051" :secure nil)))
              (format t "   ✓ Channel created~%~%")

              (unwind-protect
                  (progn
                    ;;  Wait for connection to stabilize
                    (format t "3. Waiting for connection to stabilize (2s)...~%")
                    (sleep 2)
                    (format t "   ✓ Connection stable~%~%")

                    ;; Encode request
                    (format t "4. Encoding HelloRequest...~%")
                    (let ((request (clgrpc.grpc:encode-hello-request "CL-gRPC")))
                      (format t "   Request: ~D bytes~%~%" (length request))

                      ;; Make call
                      (format t "5. Calling SayHello...~%")
                      (let ((response (clgrpc:call-unary channel
                                                         "helloworld.Greeter"
                                                         "SayHello"
                                                         request
                                                         :timeout 5000)))
                        (format t "   ✓ Response: ~D bytes~%~%" (length response))

                        ;; Decode response
                        (format t "6. Decoding HelloReply...~%")
                        (let ((message (clgrpc.grpc:decode-hello-reply response)))
                          (format t "   ✓ Message: ~A~%~%" message)
                          (format t "~%✓ SUCCESS: RPC call completed!~%")))))

                (format t "~%7. Closing channel...~%")
                (clgrpc:close-channel channel))))

        (error (e)
          (format t "~%✗ ERROR: ~A~%" e)))

    ;; Cleanup
    (format t "~%8. Stopping server...~%")
    (uiop:terminate-process server)))

(format t "Done.~%")
