;;;; client-stub.lisp - HelloWorld gRPC client using client stub
;;;;
;;;; Demonstrates automatic stub generation from server service definition.
;;;; Note: This file must be loaded after server-clos.lisp which defines greeter-service.
;;;;
;;;; Usage:
;;;;   (ql:quickload :clgrpc-examples)
;;;;   (clgrpc-examples:helloworld-stub-main)

(in-package #:clgrpc-examples)

;;; Generate client stub from server service definition
;;; This introspects the greeter-service class and generates:
;;; - A greeter-stub class
;;; - A say-hello method that handles serialization automatically

(defstub greeter-stub greeter-service)

;;; Main

(defun helloworld-stub-main ()
  "Run HelloWorld client using generated stub."
  (format t "~%HelloWorld gRPC Client (Client Stub)~%")
  (format t "======================================~%~%")
  (let ((channel (make-channel "localhost:50051" :secure nil)))
    (unwind-protect
         (handler-case
             (let ((stub (make-instance 'greeter-stub :channel channel)))
               (format t "Calling SayHello with different names:~%~%")
               (let ((reply (say-hello stub (make-hello-request :name "World"))))
                 (format t "Response: ~A~%~%" (hello-reply-message reply)))
               (let ((reply (say-hello stub (make-hello-request :name "Alice"))))
                 (format t "Response: ~A~%~%" (hello-reply-message reply)))
               (let ((reply (say-hello stub (make-hello-request :name "Bob"))))
                 (format t "Response: ~A~%~%" (hello-reply-message reply)))
               (format t "All calls completed successfully!~%"))
           (grpc-error (e)
             (format t "~%gRPC Error: ~A (status: ~D)~%"
                     (grpc-error-message e)
                     (grpc-error-status-code e)))
           (error (e)
             (format t "~%Error: ~A~%" e)))
      (close-channel channel))))
