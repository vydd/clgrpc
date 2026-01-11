;;;; client-stub.lisp - HelloWorld gRPC client using client stub
;;;;
;;;; Demonstrates automatic stub generation from server service definition

;; Load dependencies
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload '(:cl+ssl :usocket :bordeaux-threads :babel :alexandria) :silent t)

;; Add current directory to ASDF registry
(push (make-pathname :name nil :type nil
                     :defaults (merge-pathnames "../../" *load-truename*))
      asdf:*central-registry*)

;; Load the system
(format t "~%Loading clgrpc...~%")
(asdf:load-system :clgrpc)

;; Load the server-side service definition (for introspection)
(load (merge-pathnames "server-clos.lisp" *load-truename*))

(in-package #:clgrpc.client)

;;; Generate client stub from server service definition

;; This introspects the greeter-service class and generates:
;; - A greeter-stub class
;; - A say-hello method that handles serialization automatically
(defstub greeter-stub clgrpc.grpc::greeter-service)

;;; Use the stub

(defun main ()
  "Run HelloWorld client using generated stub."
  (format t "~%HelloWorld gRPC Client (Client Stub)~%")
  (format t "======================================~%~%")

  (let ((channel (make-channel "localhost:50051" :secure nil)))
    (unwind-protect
         (handler-case
             (progn
               ;; Create the stub
               (let ((stub (make-instance 'greeter-stub :channel channel)))

                 ;; Make calls - super clean syntax!
                 (format t "Calling SayHello with different names:~%~%")

                 (let ((reply (say-hello stub (clgrpc.grpc:make-hello-request :name "World"))))
                   (format t "Response: ~A~%~%" (clgrpc.grpc:hello-reply-message reply)))

                 (let ((reply (say-hello stub (clgrpc.grpc:make-hello-request :name "Alice"))))
                   (format t "Response: ~A~%~%" (clgrpc.grpc:hello-reply-message reply)))

                 (let ((reply (say-hello stub (clgrpc.grpc:make-hello-request :name "Bob"))))
                   (format t "Response: ~A~%~%" (clgrpc.grpc:hello-reply-message reply)))

                 (format t "All calls completed successfully!~%")))

           (clgrpc.grpc:grpc-error (e)
             (format t "~%gRPC Error: ~A (status: ~D)~%"
                     (clgrpc.grpc:grpc-error-message e)
                     (clgrpc.grpc:grpc-error-status-code e)))

           (error (e)
             (format t "~%Error: ~A~%" e)))

      (close-channel channel))))

;; Run if called with --run
(when (member "--run" sb-ext:*posix-argv* :test #'string=)
  (main))
