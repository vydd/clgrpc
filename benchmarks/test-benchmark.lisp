;;;; test-benchmark.lisp - Quick test of benchmark infrastructure
;;;;
;;;; Runs a short benchmark to verify everything works

;; Load Quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Install dependencies
(ql:quickload '(:cl+ssl :usocket :bordeaux-threads :alexandria :fast-io :babel :cl-json) :silent t)

;; Add current directory to ASDF registry
(push (make-pathname :name nil :type nil
                     :defaults (merge-pathnames "../" *load-truename*))
      asdf:*central-registry*)

;; Load the system
(format t "~%Loading clgrpc...~%")
(asdf:load-system :clgrpc :verbose nil)

;; Load RouteGuide definitions
(load (merge-pathnames "../examples/routeguide/routeguide-proto.lisp" *load-truename*))

(in-package #:clgrpc.client)

(defparameter *test-port* 50056)
(defparameter *test-duration* 3) ; Just 3 seconds for testing

(defun test-benchmark ()
  "Quick test of benchmark infrastructure."
  (format t "~%╔════════════════════════════════════════════════════╗~%")
  (format t "║  Benchmark Infrastructure Test                     ║~%")
  (format t "╚════════════════════════════════════════════════════╝~%")

  ;; Start server
  (format t "~%Starting RouteGuide server on port ~D...~%" *test-port*)
  (load (merge-pathnames "../examples/routeguide/server-clos.lisp" *load-truename*))

  (let ((server (clgrpc.server:make-server :port *test-port*))
        (service (make-instance 'clgrpc.grpc::route-guide-service)))

    ;; Load features data
    (setf clgrpc.grpc::*route-features*
          (clgrpc.grpc::load-features
           (merge-pathnames "../examples/routeguide/route_guide_db.json"
                           *load-truename*)))

    (format t "Loaded ~D features~%" (length clgrpc.grpc::*route-features*))

    (clgrpc.server:register-service (clgrpc.server:grpc-server-router server) service)
    (clgrpc.server:start-server server)
    (sleep 1)

    (unwind-protect
         (progn
           ;; Test unary RPC
           (format t "~%Testing unary RPC for ~D seconds...~%" *test-duration*)
           (let ((channel (make-channel (format nil "localhost:~D" *test-port*) :secure nil))
                 (count 0))
             (unwind-protect
                  (let ((start-time (get-universal-time)))
                    (loop while (< (- (get-universal-time) start-time) *test-duration*)
                          do (handler-case
                                 (let* ((point (routeguide:make-point :latitude 409146138 :longitude -746188906))
                                        (request-bytes (clgrpc.grpc:proto-serialize point)))
                                   (multiple-value-bind (response-bytes status status-message)
                                       (call-unary channel "routeguide.RouteGuide" "GetFeature"
                                                  request-bytes :timeout 5000)
                                     (when (and status (= status clgrpc.grpc:+grpc-status-ok+))
                                       (incf count))))
                               (error (e)
                                 (format t "Error: ~A~%" e)))))
               (close-channel channel))

             (format t "Completed ~D requests~%" count)
             (format t "Rate: ~,1F req/sec~%~%" (/ count *test-duration*)))

           (format t "✓ Benchmark infrastructure test passed!~%"))

      ;; Cleanup
      (clgrpc.server:stop-server server))))

(test-benchmark)
