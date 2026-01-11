;;;; profile-10-requests.lisp - Profile 10 unary requests from 1 client

(ql:quickload :clgrpc-examples :silent t)
(require :sb-sprof)

(in-package :clgrpc-examples)

;; Load route guide database
(defparameter *route-features-clos*
  (routeguide-clos-load-features
   #P"/home/vydd/Code/clgrpc/examples/routeguide/route_guide_db.json"))

;; Create and start server on port 50055
(defparameter *test-server* (make-server :port 50055))
(let ((route-guide (make-instance 'route-guide-service-clos)))
  (register-service (grpc-server-router *test-server*) route-guide))

(format t "Starting server on port 50055...~%")
(start-server *test-server*)
(format t "Server ready!~%~%")

;; Wait a bit for server to be fully ready
(sleep 2)

(format t "Starting profiler and waiting for 10 requests...~%")
(format t "Run: ./test-10-requests~%~%")

;; Start profiling
(sb-sprof:start-profiling :max-samples 100000
                          :mode :cpu
                          :sample-interval 0.001)

;; Wait for requests to complete (10 requests * ~40ms = ~400ms + buffer)
(sleep 5)

;; Stop profiling
(sb-sprof:stop-profiling)

(format t "~%Profiling complete. Generating report...~%~%")

;; Generate report to stdout
(sb-sprof:report :type :flat :max 50)

(format t "~%~%")

;; Stop server and exit
(stop-server *test-server*)
(format t "~%Done!~%")
(sb-ext:exit :code 0)
