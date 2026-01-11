;;;; debug-server.lisp - Start server with debug logging

(ql:quickload :clgrpc-examples :silent t)

;; Enable debug logging
(setf clgrpc.http2:*debug* t)

(format t "Debug logging ENABLED~%~%")

(in-package :clgrpc-examples)

;; Load database
(defparameter *route-features-clos*
  (routeguide-clos-load-features
   #P"/home/vydd/Code/clgrpc/examples/routeguide/route_guide_db.json"))

(format t "Loaded ~D features~%~%" (length *route-features-clos*))

;; Create and start server
(defparameter *server* (make-server :port 50055))
(let ((route-guide (make-instance 'route-guide-service-clos)))
  (register-service (grpc-server-router *server*) route-guide))

(format t "Starting server on port 50055 with DEBUG logging...~%")
(start-server *server*)
(format t "Server ready! Waiting for requests...~%~%")

(loop (sleep 1))
