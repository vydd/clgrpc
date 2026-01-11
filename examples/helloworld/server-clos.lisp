;;;; server-clos.lisp - HelloWorld gRPC server using CLOS-based API
;;;;
;;;; Demonstrates the CLOS-based service definition.
;;;;
;;;; Usage:
;;;;   (ql:quickload :clgrpc-examples)
;;;;   (clgrpc-examples:helloworld-server-main)

(in-package #:clgrpc-examples)

;;; Define the Greeter service using CLOS

(defclass greeter-service (grpc-service)
  ()
  (:metaclass grpc-service-metaclass)
  (:service-name "helloworld.Greeter")
  (:package "helloworld")
  (:documentation "The greeting service definition"))

;; Define the SayHello method
(defgrpc-method say-hello ((service greeter-service)
                           (request hello-request))
  (:documentation "Sends a greeting")
  (declare (ignore service))
  (let ((name (hello-request-name request)))
    (format *error-output* "Received request from: ~A~%" name)
    (make-hello-reply :message (format nil "Hello ~A!" name))))

;;; Main

(defun helloworld-server-main ()
  "Start the HelloWorld gRPC server."
  (format t "~%HelloWorld gRPC Server (CLOS-based API)~%")
  (format t "========================================~%~%")
  (let ((server (make-server :port 50051)))
    (let ((greeter (make-instance 'greeter-service)))
      (register-service (grpc-server-router server) greeter))
    (format t "~%")
    (start-server server)
    (format t "~%Server ready to accept requests.~%")
    (format t "Press Ctrl+C to stop~%~%")
    (handler-case
        (loop (sleep 1))
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl ccl:interrupt-signal-condition
       #-(or sbcl ccl) error ()
        (format t "~%Stopping server...~%")
        (stop-server server)
        (format t "Server stopped~%")))))
