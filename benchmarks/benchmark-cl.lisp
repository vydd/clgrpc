;;;; benchmark-cl.lisp - Performance benchmarks for Common Lisp gRPC
;;;;
;;;; Measures requests per second across different RPC types and client counts
;;;;
;;;; Prerequisites: (ql:quickload :clgrpc-examples)
;;;; Usage: sbcl --load benchmark-cl.lisp

(ql:quickload '(:clgrpc-examples :cl-json) :silent t)

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

(defun benchmark-unary-worker (client-id duration stop-flag results-lock results-list)
  "Worker for unary RPC benchmark."
  (declare (ignore stop-flag))
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
                          (declare (ignore status-message))
                          (if (and status (= status clgrpc.grpc:+grpc-status-ok+))
                              (incf request-count)
                              (incf error-count))))
                    (error (e)
                      (declare (ignore e))
                      (incf error-count))))

      (close-channel channel))

    ;; Record results
    (bt:with-lock-held (results-lock)
      (push (list :client-id client-id
                  :requests request-count
                  :errors error-count)
            (symbol-value results-list)))))

(defun benchmark-server-streaming-worker (client-id duration stop-flag results-lock results-list)
  "Worker for server streaming RPC benchmark."
  (declare (ignore stop-flag))
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
                             (request-bytes (clgrpc.grpc:proto-serialize rect)))
                        (multiple-value-bind (stream status status-message)
                            (call-server-streaming channel "routeguide.RouteGuide" "ListFeatures"
                                                  request-bytes :timeout 10000)
                          (declare (ignore status status-message))
                          (when stream
                            ;; Read all responses
                            (loop for response-bytes = (stream-recv stream)
                                  while response-bytes
                                  do (incf request-count))))
                    (error (e)
                      (declare (ignore e))
                      (incf error-count))))

      (close-channel channel))

    (bt:with-lock-held (results-lock)
      (push (list :client-id client-id
                  :requests request-count
                  :errors error-count)
            (symbol-value results-list)))))

(defun benchmark-client-streaming-worker (client-id duration stop-flag results-lock results-list)
  "Worker for client streaming RPC benchmark."
  (declare (ignore stop-flag))
  (let ((channel (make-channel (format nil "localhost:~D" *benchmark-port*) :secure nil))
        (request-count 0)
        (error-count 0)
        (start-time (get-universal-time)))

    (unwind-protect
         (loop while (< (- (get-universal-time) start-time) duration)
               do (handler-case
                      (multiple-value-bind (stream status status-message)
                          (call-client-streaming channel "routeguide.RouteGuide" "RecordRoute"
                                                :timeout 10000)
                        (declare (ignore status status-message))
                        (when stream
                          ;; Send 10 points
                          (dotimes (i 10)
                            (let* ((point (routeguide:make-point
                                          :latitude (+ 400000000 (* i 1000000))
                                          :longitude (- -740000000 (* i 1000000))))
                                   (point-bytes (clgrpc.grpc:proto-serialize point)))
                              (stream-send stream point-bytes)))
                          ;; Close send side and get response
                          (stream-close-send stream)
                          (let ((response-bytes (stream-recv stream)))
                            (when response-bytes
                              (incf request-count)))))
                    (error (e)
                      (declare (ignore e))
                      (incf error-count))))

      (close-channel channel))

    (bt:with-lock-held (results-lock)
      (push (list :client-id client-id
                  :requests request-count
                  :errors error-count)
            (symbol-value results-list)))))

(defun benchmark-bidi-streaming-worker (client-id duration stop-flag results-lock results-list)
  "Worker for bidirectional streaming RPC benchmark."
  (declare (ignore stop-flag))
  (let ((channel (make-channel (format nil "localhost:~D" *benchmark-port*) :secure nil))
        (request-count 0)
        (error-count 0)
        (start-time (get-universal-time)))

    (unwind-protect
         (loop while (< (- (get-universal-time) start-time) duration)
               do (handler-case
                      (multiple-value-bind (stream status status-message)
                          (call-bidirectional-streaming channel "routeguide.RouteGuide" "RouteChat"
                                                       :timeout 10000)
                        (declare (ignore status status-message))
                        (when stream
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
                                do (incf request-count))))
                    (error (e)
                      (declare (ignore e))
                      (incf error-count))))

      (close-channel channel))

    (bt:with-lock-held (results-lock)
      (push (list :client-id client-id
                  :requests request-count
                  :errors error-count)
            (symbol-value results-list)))))

;;; Benchmark Runner

(defun run-benchmark (rpc-type num-clients duration)
  "Run a benchmark for a specific RPC type and client count."
  (format t "~%Running ~A benchmark with ~D client~:P for ~D seconds...~%"
          rpc-type num-clients duration)

  (let ((results-list-sym (gensym "RESULTS-"))
        (results-lock (bt:make-lock "results-lock"))
        (stop-flag nil)
        (threads nil)
        (worker-fn (ecase rpc-type
                     (:unary #'benchmark-unary-worker)
                     (:server-streaming #'benchmark-server-streaming-worker)
                     (:client-streaming #'benchmark-client-streaming-worker)
                     (:bidi-streaming #'benchmark-bidi-streaming-worker))))

    (setf (symbol-value results-list-sym) nil)

    ;; Start worker threads
    (dotimes (i num-clients)
      (let ((client-id i))
        (push (bt:make-thread
               (lambda ()
                 (funcall worker-fn client-id duration stop-flag results-lock results-list-sym))
               :name (format nil "benchmark-worker-~D" i))
              threads)))

    ;; Wait for all threads to complete
    (dolist (thread threads)
      (bt:join-thread thread))

    ;; Aggregate results
    (let ((total-requests 0)
          (total-errors 0))
      (dolist (result (symbol-value results-list-sym))
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
  "Run complete benchmark suite.
Note: Requires a RouteGuide server running on port 50054.
Start one with: (clgrpc-examples:routeguide-server-main) on port 50054"
  (format t "~%╔════════════════════════════════════════════════════╗~%")
  (format t "║  Common Lisp gRPC Performance Benchmark           ║~%")
  (format t "╚════════════════════════════════════════════════════╝~%")
  (format t "~%NOTE: This benchmark requires a RouteGuide server running on port ~D~%"
          *benchmark-port*)
  (format t "      Start one in a separate REPL:~%")
  (format t "        (ql:quickload :clgrpc-examples)~%")
  (format t "        (let ((clgrpc-examples::*route-features* ...)) ...)~%~%")

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
  (let ((results-file (asdf:system-relative-pathname :clgrpc "benchmarks/results-cl.json")))
    (with-open-file (out results-file
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
    (format t "~%Results saved to ~A~%" results-file)))

;; Don't auto-run - user needs to start server first
(format t "~%Benchmark loaded. To run:~%")
(format t "  1. Start RouteGuide server on port ~D in another REPL~%" *benchmark-port*)
(format t "  2. Call (clgrpc.client::run-all-benchmarks)~%")
