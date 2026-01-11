;;;; server.lisp - Common Lisp gRPC server for interop testing
;;;;
;;;; This server implements the HelloWorld service for Go/CL clients.
;;;;
;;;; Prerequisites: (ql:quickload :clgrpc)
;;;; Usage: sbcl --load server.lisp --eval '(helloworld-server:main)' --quit

(ql:quickload :clgrpc :silent t)

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
     rpc SayHello (HelloRequest) returns (HelloReply)

   NOTE: request-bytes are already decoded from gRPC framing by the server.
   This handler receives raw protobuf bytes and must return raw protobuf bytes."

  (declare (ignore context))

  (format t "~%[~A] Received RPC: ~A/~A~%"
          (get-universal-time)
          service-name
          method-name)
  (format t "  Request protobuf: ~D bytes~%" (length request-bytes))

  (cond
    ((string= method-name "SayHello")
     ;; Decode HelloRequest protobuf message (already unwrapped from gRPC framing)
     (let ((name (clgrpc.grpc:decode-hello-request request-bytes)))
       (format t "  HelloRequest { name: ~S }~%" name)

       ;; Build response
       (let* ((reply-message (format nil "Hello ~A" name))
              (reply-protobuf (clgrpc.grpc:encode-hello-reply reply-message)))

         (format t "  HelloReply { message: ~S }~%" reply-message)
         (format t "  Response protobuf: ~D bytes~%~%" (length reply-protobuf))

         ;; Return: (values response-bytes status-code status-message response-metadata)
         ;; NOTE: Server will wrap response in gRPC message framing
         (values reply-protobuf
                 clgrpc.grpc:+grpc-status-ok+
                 nil
                 nil))))

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

    ;; Register reflection service (for grpc_cli and grpcurl)
    (format t "Registering reflection service...~%")
    (clgrpc.server:register-reflection-service server)
    (format t "Reflection service registered!~%~%")

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
  ;; Default to port 50051
  (run-server :port 50051))

;; Run if loaded as script
(when (member "--run" sb-ext:*posix-argv* :test #'string=)
  (main))
