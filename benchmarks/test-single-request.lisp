;;;; test-single-request.lisp - Debug a single request to the CL server

(ql:quickload :clgrpc-examples :silent t)

;; Enable debug logging
(setf clgrpc.http2:*debug* t)

(in-package :clgrpc-examples)

;; Start server in background thread
(defparameter *test-server* nil)

(format t "Starting server with debug logging enabled...~%")

(setf *test-server*
      (let ((server (make-server :port 50055)))
        ;; Load features
        (setf *route-features-clos*
              (routeguide-clos-load-features
               #P"/home/vydd/Code/clgrpc/examples/routeguide/route_guide_db.json"))
        ;; Register service
        (let ((route-guide (make-instance 'route-guide-service-clos)))
          (register-service (grpc-server-router server) route-guide))
        (start-server server)
        (format t "Server ready on port 50055~%")
        server))

(format t "~%Send a request from Go client and watch the debug output...~%")
(format t "Press Ctrl+C to stop~%~%")

(handler-case
    (loop (sleep 1))
  (#+sbcl sb-sys:interactive-interrupt
   #+ccl ccl:interrupt-signal-condition
   #-(or sbcl ccl) error ()
    (format t "~%Stopping...~%")
    (stop-server *test-server*)))
