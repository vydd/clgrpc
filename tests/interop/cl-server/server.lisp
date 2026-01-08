;;;; server.lisp - Common Lisp gRPC server for interop testing
;;;;
;;;; This server implements the HelloWorld service for Go/CL clients.

(require :asdf)
(push (truename "../../../") asdf:*central-registry*)
(asdf:load-system :clgrpc)

(defpackage #:helloworld-server
  (:use #:cl))

(in-package #:helloworld-server)

;;; Handler for SayHello RPC

(defclass greeter-handler ()
  ()
  (:documentation "Handler for helloworld.Greeter service"))

(defmethod clgrpc.server:handle-unary ((handler greeter-handler)
                                        service-name
                                        method-name
                                        request-bytes
                                        context)
  "Handle unary RPC for Greeter service.

   Implements:
     rpc SayHello (HelloRequest) returns (HelloReply)"

  (declare (ignore context))

  (format t "~%[~A] Received RPC: ~A/~A~%"
          (get-universal-time)
          service-name
          method-name)

  (cond
    ((string= method-name "SayHello")
     ;; Decode gRPC-framed request
     (let ((request-protobuf (clgrpc.grpc:decode-grpc-message request-bytes)))
       (format t "  Request: ~D bytes~%" (length request-protobuf))

       ;; Decode HelloRequest protobuf message
       (let ((name (clgrpc.grpc:decode-hello-request request-protobuf)))
         (format t "  HelloRequest { name: ~S }~%" name)

         ;; Build response
         (let* ((reply-message (format nil "Hello ~A" name))
                (reply-protobuf (clgrpc.grpc:encode-hello-reply reply-message))
                (reply-grpc (clgrpc.grpc:encode-grpc-message reply-protobuf)))

           (format t "  HelloReply { message: ~S }~%" reply-message)
           (format t "  Response: ~D bytes~%~%" (length reply-grpc))

           ;; Return: (values response-bytes status-code status-message response-metadata)
           (values reply-grpc
                   clgrpc.grpc:+grpc-status-ok+
                   nil
                   nil)))))

    (t
     ;; Unknown method
     (format t "  ERROR: Unknown method~%~%")
     (values nil
             clgrpc.grpc:+grpc-status-unimplemented+
             (format nil "Method ~A not implemented" method-name)
             nil))))

;;; Server Setup

(defun run-server (&key (port 50051))
  "Run the HelloWorld gRPC server.

   Args:
     port: Port to listen on (default: 50051)"

  (format t "~%Common Lisp gRPC Server~%")
  (format t "========================~%~%")

  (let ((server (clgrpc:make-server :port port))
        (handler (make-instance 'greeter-handler)))

    (format t "Registering helloworld.Greeter service...~%")

    ;; Register handler for the service
    (clgrpc.server:register-handler
     (clgrpc.server:grpc-server-router server)
     "helloworld.Greeter"
     "SayHello"
     handler)

    (format t "Service registered!~%~%")
    (format t "Starting server on port ~D...~%" port)

    ;; Start server
    (clgrpc:start-server server)

    (format t "~%Server is listening!~%")
    (format t "========================~%")
    (format t "~%Press Ctrl+C to stop~%~%")

    ;; Keep server running
    (handler-case
        (loop (sleep 1))
      (sb-sys:interactive-interrupt ()
        (format t "~%~%Shutting down...~%")
        (clgrpc:stop-server server)
        (format t "Server stopped.~%~%")))))

(defun main ()
  "Entry point for the server"
  (let ((port-str (second sb-ext:*posix-argv*)))
    (if port-str
        (run-server :port (parse-integer port-str :junk-allowed t))
        (run-server))))

;; Run if loaded as script
(when (member "--run" sb-ext:*posix-argv* :test #'string=)
  (main))
