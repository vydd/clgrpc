;;;; client.lisp - Common Lisp gRPC client for interop testing
;;;;
;;;; This client calls a gRPC server (Go or CL) with a HelloWorld request.

(require :asdf)
(push (truename "../../../") asdf:*central-registry*)
(asdf:load-system :clgrpc)

(defpackage #:helloworld-client
  (:use #:cl))

(in-package #:helloworld-client)

(defun run-client (&key (host "localhost") (port 50051) (name "world"))
  "Run the HelloWorld client and make a SayHello RPC call.

   Args:
     host: Server hostname (default: localhost)
     port: Server port (default: 50051)
     name: Name to send in greeting (default: world)

   Returns:
     The greeting message received from server"

  (format t "~%Common Lisp gRPC Client~%")
  (format t "========================~%~%")

  (let* ((target (format nil "~A:~D" host port))
         (channel nil)
         (greeting nil))

    (unwind-protect
        (handler-case
            (progn
              (format t "Connecting to ~A...~%" target)

              ;; Create channel
              (setf channel (clgrpc:make-channel target :secure nil))
              (format t "Channel created!~%~%")

              ;; Encode HelloRequest protobuf message
              (format t "Sending HelloRequest { name: ~S }~%" name)
              (let ((request-bytes (clgrpc.grpc:encode-hello-request name)))
                (format t "  Encoded to ~D bytes~%" (length request-bytes))

                ;; Make unary RPC call
                (format t "Calling helloworld.Greeter/SayHello...~%")
                (multiple-value-bind (response-grpc-data status status-message)
                    (clgrpc:call-unary channel
                                      "helloworld.Greeter"
                                      "SayHello"
                                      request-bytes
                                      :timeout 5000)  ; 5 second timeout

                  (format t "  gRPC status: ~D~A~%"
                          status
                          (if (and status-message (not (string= status-message "")))
                              (format nil " (~A)" status-message)
                              ""))

                  ;; Decode gRPC-framed response
                  (when response-grpc-data
                    (let ((response-bytes (clgrpc.grpc:decode-grpc-message response-grpc-data)))
                      (format t "  Response: ~D bytes~%" (length response-bytes))

                      ;; Decode HelloReply protobuf message
                      (setf greeting (clgrpc.grpc:decode-hello-reply response-bytes))
                      (format t "~%HelloReply { message: ~S }~%" greeting))))))

          (clgrpc.grpc:grpc-error (e)
            (format t "~%gRPC Error: ~A~%" (clgrpc.grpc:grpc-error-message e))
            (format t "  Status code: ~D~%" (clgrpc.grpc:grpc-error-status-code e)))

          (error (e)
            (format t "~%Error: ~A~%" e)))

      ;; Cleanup
      (when channel
        (clgrpc:close-channel channel)))

    (format t "~%========================~%")
    (if greeting
        (format t "SUCCESS: ~A~%~%" greeting)
        (format t "FAILED~%~%"))

    greeting))

(defun main ()
  "Entry point for the client"
  (let ((name (or (second sb-ext:*posix-argv*) "world")))
    (run-client :name name)))

;; Run if loaded as script
(when (member "--run" sb-ext:*posix-argv* :test #'string=)
  (main))
