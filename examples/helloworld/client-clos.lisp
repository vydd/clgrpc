;;;; client-clos.lisp - HelloWorld gRPC client using CLOS-based API
;;;;
;;;; Demonstrates the CLOS-based message API.
;;;;
;;;; Usage:
;;;;   (ql:quickload :clgrpc-examples)
;;;;   (clgrpc-examples:helloworld-client-main)

(in-package #:clgrpc-examples)

;;; Make a call to the server

(defun call-say-hello (channel name)
  "Make a SayHello RPC call."
  (let ((request (make-hello-request :name name)))
    (format t "Sending request: name=~A~%" name)
    (multiple-value-bind (response-bytes status status-message)
        (call-unary channel
                    "helloworld.Greeter"
                    "SayHello"
                    (proto-serialize request))
      (if (= status +grpc-status-ok+)
          (let ((reply (proto-deserialize 'hello-reply response-bytes)))
            (format t "Received reply: ~A~%"
                    (hello-reply-message reply)))
          (format t "Error: ~A (~D)~%" status-message status)))))

;;; Main

(defun helloworld-client-main ()
  "Run HelloWorld client."
  (format t "~%HelloWorld gRPC Client (CLOS-based API)~%")
  (format t "========================================~%~%")
  (let ((channel (make-channel "localhost:50051" :secure nil)))
    (unwind-protect
         (handler-case
             (progn
               (call-say-hello channel "World")
               (format t "~%")
               (call-say-hello channel "Alice")
               (format t "~%")
               (call-say-hello channel "Bob")
               (format t "~%All calls completed successfully!~%"))
           (error (e)
             (format t "~%Error: ~A~%" e)))
      (close-channel channel))))
