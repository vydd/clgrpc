;;;; server-clos.lisp - HelloWorld gRPC server using CLOS-based API
;;;;
;;;; Demonstrates the new CLOS-based service definition

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

(in-package #:clgrpc.grpc)

;;; Define the Greeter service using CLOS

(defclass greeter-service (grpc-service)
  ()
  (:metaclass grpc-service-metaclass)
  (:service-name "helloworld.Greeter")
  (:package "helloworld")
  (:documentation "The greeting service definition"))

;; Define the SayHello method
;; :method-name defaults to "SayHello" (CamelCase of say-hello)
;; :rpc-type defaults to :unary
(defgrpc-method say-hello ((service greeter-service)
                           (request hello-request)
                           context)
  (:documentation "Sends a greeting")

  (declare (ignore context))

  ;; Get the name from the request
  (let ((name (hello-request-name request)))
    (format *error-output* "Received request from: ~A~%" name)

    ;; Create and return the reply
    (make-hello-reply :message (format nil "Hello ~A!" name))))

;;; Start the server

(defun main ()
  "Start the HelloWorld gRPC server."
  (format t "~%HelloWorld gRPC Server (CLOS-based API)~%")
  (format t "========================================~%~%")

  ;; Create server
  (let ((server (clgrpc.server:make-server :port 50051)))

    ;; Create service instance and register it
    (let ((greeter (make-instance 'greeter-service)))
      (clgrpc.server:register-service (clgrpc.server:grpc-server-router server)
                                      greeter))

    (format t "~%")

    ;; Start server
    (clgrpc.server:start-server server)

    (format t "~%Server ready to accept requests.~%")
    (format t "Press Ctrl+C to stop~%~%")

    ;; Keep running
    (handler-case
        (loop (sleep 1))
      (sb-sys:interactive-interrupt ()
        (format t "~%Stopping server...~%")
        (clgrpc.server:stop-server server)
        (format t "Server stopped~%")))))

;; Run if called with --run
(when (member "--run" sb-ext:*posix-argv* :test #'string=)
  (main))
