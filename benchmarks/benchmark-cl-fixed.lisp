;;;; benchmark-cl-fixed.lisp - Fixed CL benchmark with simpler result collection

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

(defparameter *benchmark-port* 50054)
(defparameter *benchmark-duration* 5)
(defparameter *results* nil)

(defstruct benchmark-result
  rpc-type
  num-clients
  total-requests
  duration
  requests-per-second
  errors)

;;; Worker Functions - Return (values requests errors)

(defun benchmark-unary-worker (duration)
  "Worker for unary RPC benchmark. Returns (values request-count error-count)."
  (let ((channel (make-channel (format nil "localhost:~D" *benchmark-port*) :secure nil))
        (request-count 0)
        (error-count 0)
        (start-time (get-universal-time)))

    (unwind-protect
         (loop while (< (- (get-universal-time) start-time) duration)
               do (handler-case
                      (let* ((point (routeguide:make-point :latitude 409146138 :longitude -746188906))
                             (request-bytes (clgrpc.grpc:proto-serialize point)))
                        (multiple-value-bind (response-bytes status status-message)
                            (call-unary channel "routeguide.RouteGuide" "GetFeature"
                                       request-bytes :timeout 5000)
                          (declare (ignore response-bytes status-message))
                          (if (and status (= status clgrpc.grpc:+grpc-status-ok+))
                              (incf request-count)
                              (incf error-count))))
                    (error (e)
                      (declare (ignore e))
                      (incf error-count))))

      (close-channel channel))

    (values request-count error-count)))

(defun run-benchmark (rpc-type num-clients duration worker-fn)
  "Run a benchmark for a specific RPC type and client count."
  (format t "~%Running ~A benchmark with ~D client~:P for ~D seconds...~%"
          rpc-type num-clients duration)

  (let ((threads nil))

    ;; Start worker threads
    (dotimes (i num-clients)
      (push (bt:make-thread
             (lambda () (funcall worker-fn duration))
             :name (format nil "benchmark-worker-~D" i))
            threads))

    ;; Wait for all threads and collect results
    (let ((total-requests 0)
          (total-errors 0))
      (dolist (thread threads)
        (multiple-value-bind (requests errors)
            (bt:join-thread thread)
          (incf total-requests requests)
          (incf total-errors errors)))

      (let ((req-per-sec (if (> duration 0)
                             (/ total-requests duration)
                             0)))
        (format t "  Total requests: ~D~%" total-requests)
        (format t "  Total errors: ~D~%" total-errors)
        (format t "  Requests/sec: ~,1F~%~%" req-per-sec)

        (make-benchmark-result
         :rpc-type rpc-type
         :num-clients num-clients
         :total-requests total-requests
         :duration duration
         :requests-per-second req-per-sec
         :errors total-errors)))))

(defun run-all-benchmarks ()
  "Run complete benchmark suite."
  (format t "~%╔════════════════════════════════════════════════════╗~%")
  (format t "║  Common Lisp gRPC Performance Benchmark           ║~%")
  (format t "╚════════════════════════════════════════════════════╝~%")

  ;; Start server
  (format t "~%Starting RouteGuide server on port ~D...~%" *benchmark-port*)
  (load (merge-pathnames "../examples/routeguide/server-clos.lisp" *load-truename*))

  (let ((server (clgrpc.server:make-server :port *benchmark-port*))
        (service (make-instance 'clgrpc.grpc::route-guide-service)))

    ;; Load features data
    (setf clgrpc.grpc::*route-features*
          (clgrpc.grpc::load-features
           (merge-pathnames "../examples/routeguide/route_guide_db.json"
                           *load-truename*)))

    (clgrpc.server:register-service (clgrpc.server:grpc-server-router server) service)
    (clgrpc.server:start-server server)
    (sleep 1) ; Let server initialize

    (unwind-protect
         (progn
           (setf *results* nil)

           ;; Run benchmarks - just unary for now to test
           (dolist (num-clients '(1 10 100))
             (let ((result (run-benchmark :unary num-clients *benchmark-duration*
                                         #'benchmark-unary-worker)))
               (push result *results*)))

           ;; Print summary
           (format t "~%╔════════════════════════════════════════════════════╗~%")
           (format t "║  Benchmark Results Summary                         ║~%")
           (format t "╚════════════════════════════════════════════════════╝~%~%")

           (format t "~25A ~12A ~15A~%" "RPC Type" "Clients" "Req/Sec")
           (format t "~60,,,'-A~%" "")

           (dolist (result (reverse *results*))
             (format t "~25A ~12D ~15,1F~%"
                     (benchmark-result-rpc-type result)
                     (benchmark-result-num-clients result)
                     (benchmark-result-requests-per-second result)))

           ;; Save results to JSON file
           (with-open-file (out (merge-pathnames "results-cl.json" *load-truename*)
                               :direction :output
                               :if-exists :supersede)
             (let ((json-results
                    (mapcar (lambda (r)
                              (list (cons "rpc_type" (string-downcase
                                                     (symbol-name (benchmark-result-rpc-type r))))
                                    (cons "num_clients" (benchmark-result-num-clients r))
                                    (cons "requests_per_second" (benchmark-result-requests-per-second r))
                                    (cons "total_requests" (benchmark-result-total-requests r))
                                    (cons "errors" (benchmark-result-errors r))))
                            (reverse *results*))))
               (format out "~A~%" (cl-json:encode-json-to-string json-results))))

           (format t "~%Results saved to results-cl.json~%"))

      ;; Cleanup
      (clgrpc.server:stop-server server))))

;; Run benchmarks
(run-all-benchmarks)
