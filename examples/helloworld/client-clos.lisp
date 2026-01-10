;;;; client-clos.lisp - HelloWorld gRPC client using CLOS-based API
;;;;
;;;; Demonstrates the new CLOS-based message API

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

(in-package #:clgrpc.client)

;;; Make a call to the server

(defun call-say-hello (name)
  "Make a SayHello RPC call."
  (let ((channel (make-channel "localhost:50051" :secure nil)))
    (unwind-protect
         (progn
           ;; Create request using CLOS API
           (let ((request (clgrpc.grpc:make-hello-request :name name)))

             (format t "Sending request: name=~A~%" name)

             ;; Make the call - returns bytes
             (multiple-value-bind (response-bytes status status-message)
                 (call-unary channel
                            "helloworld.Greeter"
                            "SayHello"
                            (clgrpc.grpc:proto-serialize request))

               (if (= status clgrpc.grpc:+grpc-status-ok+)
                   ;; Deserialize response using CLOS API
                   (let ((reply (clgrpc.grpc:proto-deserialize 'clgrpc.grpc:hello-reply
                                                                response-bytes)))
                     (format t "Received reply: ~A~%"
                             (clgrpc.grpc:hello-reply-message reply)))
                   (format t "Error: ~A (~D)~%" status-message status)))))
      (close-channel channel))))

;;; Main

(defun main ()
  "Run HelloWorld client."
  (format t "~%HelloWorld gRPC Client (CLOS-based API)~%")
  (format t "========================================~%~%")

  (handler-case
      (progn
        ;; Test with different names
        (call-say-hello "World")
        (format t "~%")
        (call-say-hello "Alice")
        (format t "~%")
        (call-say-hello "Bob")

        (format t "~%All calls completed successfully!~%"))

    (error (e)
      (format t "~%Error: ~A~%" e))))

;; Run if called with --run
(when (member "--run" sb-ext:*posix-argv* :test #'string=)
  (main))
