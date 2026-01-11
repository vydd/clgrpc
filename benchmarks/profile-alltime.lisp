;;;; profile-alltime.lisp - Profile with :time mode to catch I/O waits

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

(format t "Server will run with profiling for 8 seconds.~%")
(format t "Using :mode :time to catch I/O waits!~%")
(format t "Run your test client now!~%~%")

;; Start profiling with TIME mode (not CPU mode)
(sb-sprof:start-profiling :max-samples 1000000
                          :mode :time
                          :sample-interval 0.0001
                          :threads :all)

;; Wait for requests
(sleep 8)

;; Stop profiling
(sb-sprof:stop-profiling)

(format t "~%Profiling complete. Generating report...~%~%")

;; Generate report to stdout - show top 100 to see clgrpc functions
(sb-sprof:report :type :flat :max 100)

(format t "~%~%")

;; Stop server and exit
(stop-server *test-server*)
(format t "~%Done!~%")
(sb-ext:exit :code 0)
