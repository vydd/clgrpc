;;;; profile-hotspots.lisp - Profile gRPC to find performance bottlenecks

(ql:quickload '(:clgrpc :sb-sprof) :silent t)

(defparameter *port* (+ 55000 (random 1000)))
(defparameter *profile-seconds* 5)

(format t "~%gRPC Performance Profiling~%")
(format t "==========================~%~%")

;; Create and start server
(let ((server (clgrpc.server:make-server :port *port*)))
  (clgrpc.server:register-handler
   (clgrpc.server:grpc-server-router server)
   "test.Echo" "Echo"
   (clgrpc.server:lambda-handler
     (values clgrpc.server::request-bytes 0 nil nil))
   :rpc-type :unary)

  (clgrpc.server:start-server server)
  (sleep 0.3)
  (format t "Server started on port ~D~%~%" *port*)

  (let ((channel (clgrpc.client:make-channel (format nil "localhost:~D" *port*) :secure nil))
        (message (babel:string-to-octets "Hello benchmark test message for profiling")))

    (unwind-protect
        (progn
          ;; Warm up
          (format t "Warming up...~%")
          (dotimes (i 1000)
            (clgrpc.client:call-unary channel "test.Echo" "Echo" message))

          ;; Profile
          (format t "Profiling for ~D seconds...~%~%" *profile-seconds*)
          (sb-sprof:with-profiling (:max-samples 10000
                                    :report :flat
                                    :loop nil)
            (let ((start (get-internal-real-time))
                  (count 0))
              (loop while (< (/ (- (get-internal-real-time) start)
                               internal-time-units-per-second)
                            *profile-seconds*)
                    do (clgrpc.client:call-unary channel "test.Echo" "Echo" message)
                       (incf count))
              (format t "~%Completed ~D requests in ~D seconds (~,1F req/sec)~%~%"
                      count *profile-seconds* (/ count *profile-seconds*)))))
      (clgrpc.client:close-channel channel)))

  (clgrpc.server:stop-server server :timeout 2))

(sb-ext:exit :code 0)
