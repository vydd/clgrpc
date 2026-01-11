;;;; benchmark-cl.lisp - Performance benchmarks for Common Lisp gRPC
;;;;
;;;; Measures requests per second across different RPC types and client counts

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
(defparameter *benchmark-duration* 5) ; seconds per test
(defparameter *results* nil)

;;; Benchmark Configuration

(defstruct benchmark-result
  rpc-type
  num-clients
  total-requests
  duration
  requests-per-second
  errors)

;;; Worker Functions for Each RPC Type

(defun benchmark-unary-worker (client-id duration results-lock results-list-place)
  "Worker for unary RPC benchmark."
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
                          (if (and status (= status clgrpc.grpc:+grpc-status-ok+))
                              (incf request-count)
                              (incf error-count))))
                    (error (e)
                      (incf error-count))))

      (close-channel channel))

    ;; Record results
    (bt:with-lock-held (results-lock)
      (setf (car results-list-place)
            (cons (list :client-id client-id
                        :requests request-count
                        :errors error-count)
                  (car results-list-place))))))

(defun benchmark-server-streaming-worker (client-id duration stop-flag results-lock results-list)
  "Worker for server streaming RPC benchmark."
  (let ((channel (make-channel (format nil "localhost:~D" *benchmark-port*) :secure nil))
        (request-count 0)
        (error-count 0)
        (start-time (get-universal-time)))

    (unwind-protect
         (loop while (< (- (get-universal-time) start-time) duration)
               do (handler-case
                      (let* ((rect (routeguide:make-rectangle
                                   :lo (routeguide:make-point :latitude 400000000 :longitude -750000000)
                                   :hi (routeguide:make-point :latitude 420000000 :longitude -730000000)))
                             (request-bytes (clgrpc.grpc:proto-serialize rect))
                             (stream (call-server-streaming channel "routeguide.RouteGuide" "ListFeatures"
                                                           request-bytes :timeout 10000)))
                        ;; Read all responses
                        (loop for response-bytes = (stream-recv stream)
                              while response-bytes
                              do (incf request-count))
                        (stream-close stream))
                    (error (e)
                      (incf error-count))))

      (close-channel channel))

    (bt:with-lock-held (results-lock)
      (push (list :client-id client-id
                  :requests request-count
                  :errors error-count)
            results-list))))

(defun benchmark-client-streaming-worker (client-id duration stop-flag results-lock results-list)
  "Worker for client streaming RPC benchmark."
  (let ((channel (make-channel (format nil "localhost:~D" *benchmark-port*) :secure nil))
        (request-count 0)
        (error-count 0)
        (start-time (get-universal-time)))

    (unwind-protect
         (loop while (< (- (get-universal-time) start-time) duration)
               do (handler-case
                      (let ((stream (call-client-streaming channel "routeguide.RouteGuide" "RecordRoute"
                                                          :timeout 10000)))
                        ;; Send 10 points
                        (dotimes (i 10)
                          (let* ((point (routeguide:make-point
                                        :latitude (+ 400000000 (* i 1000000))
                                        :longitude (- -740000000 (* i 1000000))))
                                 (point-bytes (clgrpc.grpc:proto-serialize point)))
                            (stream-send stream point-bytes)))
                        (stream-close-send stream)
                        ;; Get response
                        (let ((response-bytes (stream-recv stream)))
                          (when response-bytes
                            (incf request-count)))
                        (stream-close stream))
                    (error (e)
                      (incf error-count))))

      (close-channel channel))

    (bt:with-lock-held (results-lock)
      (push (list :client-id client-id
                  :requests request-count
                  :errors error-count)
            results-list))))

(defun benchmark-bidi-streaming-worker (client-id duration stop-flag results-lock results-list)
  "Worker for bidirectional streaming RPC benchmark."
  (let ((channel (make-channel (format nil "localhost:~D" *benchmark-port*) :secure nil))
        (request-count 0)
        (error-count 0)
        (start-time (get-universal-time)))

    (unwind-protect
         (loop while (< (- (get-universal-time) start-time) duration)
               do (handler-case
                      (let ((stream (call-bidi-streaming channel "routeguide.RouteGuide" "RouteChat"
                                                        :timeout 10000)))
                        ;; Send 5 notes
                        (dotimes (i 5)
                          (let* ((note (routeguide:make-route-note
                                       :location (routeguide:make-point
                                                 :latitude (+ 400000000 (* i 1000000))
                                                 :longitude (- -740000000 (* i 1000000)))
                                       :message (format nil "Message ~D from client ~D" i client-id)))
                                 (note-bytes (clgrpc.grpc:proto-serialize note)))
                            (stream-send stream note-bytes)))
                        (stream-close-send stream)
                        ;; Read all responses
                        (loop for response-bytes = (stream-recv stream)
                              while response-bytes
                              do (incf request-count))
                        (stream-close stream))
                    (error (e)
                      (incf error-count))))

      (close-channel channel))

    (bt:with-lock-held (results-lock)
      (push (list :client-id client-id
                  :requests request-count
                  :errors error-count)
            results-list))))

;;; Benchmark Runner

(defun run-benchmark (rpc-type num-clients duration)
  "Run a benchmark for a specific RPC type and client count."
  (format t "~%Running ~A benchmark with ~D client~:P for ~D seconds...~%"
          rpc-type num-clients duration)

  (let ((results-list nil)
        (results-lock (bt:make-lock "results-lock"))
        (stop-flag nil)
        (threads nil)
        (worker-fn (ecase rpc-type
                     (:unary #'benchmark-unary-worker)
                     (:server-streaming #'benchmark-server-streaming-worker)
                     (:client-streaming #'benchmark-client-streaming-worker)
                     (:bidi-streaming #'benchmark-bidi-streaming-worker))))

    ;; Start worker threads
    (dotimes (i num-clients)
      (push (bt:make-thread
             (lambda ()
               (funcall worker-fn i duration stop-flag results-lock results-list))
             :name (format nil "benchmark-worker-~D" i))
            threads))

    ;; Wait for all threads to complete
    (dolist (thread threads)
      (bt:join-thread thread))

    ;; Aggregate results
    (let ((total-requests 0)
          (total-errors 0))
      (dolist (result results-list)
        (incf total-requests (getf result :requests))
        (incf total-errors (getf result :errors)))

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

;;; Main Benchmark Suite

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

           ;; Run benchmarks for each RPC type and client count
           (dolist (rpc-type '(:unary :server-streaming :client-streaming :bidi-streaming))
             (dolist (num-clients '(1 10 100))
               (let ((result (run-benchmark rpc-type num-clients *benchmark-duration*)))
                 (push result *results*))))

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
