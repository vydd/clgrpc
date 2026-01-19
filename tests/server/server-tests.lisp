;;;; server-tests.lisp - Comprehensive server tests

(in-package #:clgrpc-tests)

(def-suite server-tests
  :in clgrpc-all
  :description "gRPC server tests")

(in-suite server-tests)

;;; ============================================================
;;; Router Tests
;;; ============================================================

(test router-creation
  "Test creating a router"
  (let ((router (clgrpc.server::make-router)))
    (is (not (null router)))
    (is (typep router 'clgrpc.server::grpc-router))))

(test router-path-creation
  "Test gRPC path creation from service/method names"
  (is (string= (clgrpc.server::make-grpc-path "helloworld.Greeter" "SayHello")
               "/helloworld.Greeter/SayHello"))
  (is (string= (clgrpc.server::make-grpc-path "test.Service" "Method")
               "/test.Service/Method"))
  (is (string= (clgrpc.server::make-grpc-path "a" "b")
               "/a/b")))

(test router-path-parsing
  "Test parsing gRPC paths into service/method"
  ;; Valid paths
  (multiple-value-bind (service method)
      (clgrpc.server::parse-grpc-path "/helloworld.Greeter/SayHello")
    (is (string= service "helloworld.Greeter"))
    (is (string= method "SayHello")))

  (multiple-value-bind (service method)
      (clgrpc.server::parse-grpc-path "/test.Echo/Unary")
    (is (string= service "test.Echo"))
    (is (string= method "Unary")))

  ;; Invalid paths
  (multiple-value-bind (service method)
      (clgrpc.server::parse-grpc-path "invalid")
    (is (null service))
    (is (null method)))

  (multiple-value-bind (service method)
      (clgrpc.server::parse-grpc-path "/no-slash")
    (is (null service))
    (is (null method)))

  (multiple-value-bind (service method)
      (clgrpc.server::parse-grpc-path "")
    (is (null service))
    (is (null method))))

(test router-register-handler
  "Test registering handlers with router"
  (let ((router (clgrpc.server::make-router))
        (handler (clgrpc.server:lambda-handler
                   (values nil 0 nil nil))))
    ;; Register handler
    (clgrpc.server:register-handler router "test.Service" "Method" handler)

    ;; Route should find it
    (multiple-value-bind (found-handler service method rpc-type)
        (clgrpc.server::route-request router "/test.Service/Method")
      (is (eq found-handler handler))
      (is (string= service "test.Service"))
      (is (string= method "Method"))
      (is (eq rpc-type :unary)))))

(test router-register-streaming-handler
  "Test registering handlers with different RPC types"
  (let ((router (clgrpc.server::make-router))
        (handler (clgrpc.server:lambda-handler
                   (values nil 0 nil nil))))
    ;; Register with different RPC types
    (clgrpc.server:register-handler router "test.Service" "Unary" handler :rpc-type :unary)
    (clgrpc.server:register-handler router "test.Service" "ClientStream" handler :rpc-type :client-streaming)
    (clgrpc.server:register-handler router "test.Service" "ServerStream" handler :rpc-type :server-streaming)
    (clgrpc.server:register-handler router "test.Service" "Bidi" handler :rpc-type :bidirectional)

    ;; Check RPC types
    (multiple-value-bind (h s m rpc-type)
        (clgrpc.server::route-request router "/test.Service/Unary")
      (declare (ignore h s m))
      (is (eq rpc-type :unary)))

    (multiple-value-bind (h s m rpc-type)
        (clgrpc.server::route-request router "/test.Service/ClientStream")
      (declare (ignore h s m))
      (is (eq rpc-type :client-streaming)))

    (multiple-value-bind (h s m rpc-type)
        (clgrpc.server::route-request router "/test.Service/ServerStream")
      (declare (ignore h s m))
      (is (eq rpc-type :server-streaming)))

    (multiple-value-bind (h s m rpc-type)
        (clgrpc.server::route-request router "/test.Service/Bidi")
      (declare (ignore h s m))
      (is (eq rpc-type :bidirectional)))))

(test router-unregister-handler
  "Test unregistering handlers"
  (let ((router (clgrpc.server::make-router))
        (handler (clgrpc.server:lambda-handler
                   (values nil 0 nil nil))))
    ;; Register and verify
    (clgrpc.server:register-handler router "test.Service" "Method" handler)
    (multiple-value-bind (found-handler service method rpc-type)
        (clgrpc.server::route-request router "/test.Service/Method")
      (declare (ignore service method rpc-type))
      (is (eq found-handler handler)))

    ;; Unregister
    (clgrpc.server::unregister-handler router "test.Service" "Method")

    ;; Should now get default handler
    (multiple-value-bind (found-handler service method rpc-type)
        (clgrpc.server::route-request router "/test.Service/Method")
      (declare (ignore service method rpc-type))
      (is (not (eq found-handler handler)))
      (is (typep found-handler 'clgrpc.server::default-handler)))))

(test router-default-handler
  "Test default handler for unregistered routes"
  (let ((router (clgrpc.server::make-router)))
    ;; Unregistered route should use default handler
    (multiple-value-bind (handler service method rpc-type)
        (clgrpc.server::route-request router "/nonexistent.Service/Method")
      (is (typep handler 'clgrpc.server::default-handler))
      (is (string= service "nonexistent.Service"))
      (is (string= method "Method"))
      (is (eq rpc-type :unary)))))

(test router-bulk-registration
  "Test registering multiple handlers at once"
  (let ((router (clgrpc.server::make-router))
        (handler1 (clgrpc.server:lambda-handler (values nil 0 nil nil)))
        (handler2 (clgrpc.server:lambda-handler (values nil 0 nil nil))))
    (clgrpc.server::register-service-handlers router "test.Service"
      `(("Method1" . ,handler1)
        ("Method2" . ,handler2)))

    ;; Both should be registered
    (multiple-value-bind (h s m r)
        (clgrpc.server::route-request router "/test.Service/Method1")
      (declare (ignore s m r))
      (is (eq h handler1)))

    (multiple-value-bind (h s m r)
        (clgrpc.server::route-request router "/test.Service/Method2")
      (declare (ignore s m r))
      (is (eq h handler2)))))

;;; ============================================================
;;; Service Tests (struct-based service.lisp is deprecated/not loaded)
;;; CLOS-based services (service-clos.lisp) are tested via integration tests
;;; ============================================================

;;; ============================================================
;;; Handler Tests
;;; ============================================================

(test handler-context-creation
  "Test handler context structure"
  (let ((ctx (clgrpc.server::make-handler-context
              :stream-id 1
              :metadata '(("key" . "value"))
              :deadline 12345)))
    (is (= (clgrpc.server::handler-context-stream-id ctx) 1))
    (is (equal (clgrpc.server::handler-context-metadata ctx) '(("key" . "value"))))
    (is (= (clgrpc.server::handler-context-deadline ctx) 12345))
    (is (not (clgrpc.server::handler-context-cancelled ctx)))))

(test handler-deadline-check
  "Test deadline checking functions"
  ;; Context with past deadline
  (let ((ctx (clgrpc.server::make-handler-context
              :deadline (- (get-internal-real-time) 1000))))
    (is (clgrpc.server:deadline-exceeded-p ctx)))

  ;; Context with future deadline
  (let ((ctx (clgrpc.server::make-handler-context
              :deadline (+ (get-internal-real-time)
                          (* 10 internal-time-units-per-second)))))
    (is (not (clgrpc.server:deadline-exceeded-p ctx)))
    (let ((remaining (clgrpc.server:time-remaining-ms ctx)))
      (is (> remaining 0))
      (is (< remaining 11000))))

  ;; Context with no deadline
  (let ((ctx (clgrpc.server::make-handler-context)))
    (is (not (clgrpc.server:deadline-exceeded-p ctx)))
    (is (null (clgrpc.server:time-remaining-ms ctx)))))

(test lambda-handler-basic
  "Test lambda-handler macro"
  (let ((handler (clgrpc.server:lambda-handler
                   (values clgrpc.server::request-bytes 0 nil nil))))
    (is (typep handler 'clgrpc.server::function-handler))

    ;; Call the handler
    (let ((request (babel:string-to-octets "test")))
      (multiple-value-bind (response status msg meta)
          (clgrpc.server:handle-unary handler "svc" "method" request nil)
        (is (equalp response request))
        (is (= status 0))
        (is (null msg))
        (is (null meta))))))

(test default-handler-returns-unimplemented
  "Test default handler returns UNIMPLEMENTED"
  (let ((handler (make-instance 'clgrpc.server::default-handler)))
    (multiple-value-bind (response status msg meta)
        (clgrpc.server:handle-unary handler "test.Service" "Method" nil nil)
      (declare (ignore meta))
      (is (null response))
      (is (= status clgrpc.grpc:+grpc-status-unimplemented+))
      (is (search "not implemented" msg)))))

(test function-handler-invocation
  "Test function handler calls the underlying function"
  (let* ((called nil)
         (handler (clgrpc.server::make-function-handler
                   (lambda (service method request ctx)
                     (declare (ignore ctx))
                     (setf called t)
                     (values (concatenate '(vector (unsigned-byte 8))
                                         request request)
                             0
                             nil
                             `(("service" . ,service)
                               ("method" . ,method)))))))
    (let ((request (babel:string-to-octets "X")))
      (multiple-value-bind (response status msg meta)
          (clgrpc.server:handle-unary handler "MySvc" "MyMethod" request nil)
        (declare (ignore msg))
        (is-true called)
        (is (= (length response) 2))
        (is (= status 0))
        (is (string= (cdr (assoc "service" meta :test #'string=)) "MySvc"))
        (is (string= (cdr (assoc "method" meta :test #'string=)) "MyMethod"))))))

;;; ============================================================
;;; Interceptor Tests
;;; ============================================================

(test interceptor-info-creation
  "Test interceptor info structure"
  (let ((info (clgrpc.server::make-interceptor-info
               :service "test.Service"
               :method "Method"
               :rpc-type :unary)))
    (is (string= (clgrpc.server::interceptor-info-service info) "test.Service"))
    (is (string= (clgrpc.server::interceptor-info-method info) "Method"))
    (is (eq (clgrpc.server::interceptor-info-rpc-type info) :unary))))

(test interceptor-chain-execution
  "Test interceptor chain executes in order"
  (let ((order nil)
        (info (clgrpc.server::make-interceptor-info :service "svc" :method "m" :rpc-type :unary)))

    (flet ((interceptor-1 (req ctx info cont)
             (declare (ignore info))
             (push 1 order)
             (multiple-value-prog1
                 (funcall cont req ctx)
               (push 10 order)))
           (interceptor-2 (req ctx info cont)
             (declare (ignore info))
             (push 2 order)
             (multiple-value-prog1
                 (funcall cont req ctx)
               (push 20 order)))
           (handler (req ctx)
             (declare (ignore ctx))
             (push :handler order)
             (values req 0 nil nil)))

      (clgrpc.server::execute-unary-interceptor-chain
       (list #'interceptor-1 #'interceptor-2)
       #'handler
       (babel:string-to-octets "test")
       nil
       info)

      ;; Order should be: 1, 2, handler, 20, 10 (LIFO for post-processing)
      (is (equal (reverse order) '(1 2 :handler 20 10))))))

(test interceptor-can-short-circuit
  "Test interceptor can short-circuit without calling handler"
  (let ((handler-called nil)
        (info (clgrpc.server::make-interceptor-info :service "svc" :method "m" :rpc-type :unary)))

    (flet ((auth-interceptor (req ctx info cont)
             (declare (ignore req ctx info cont))
             ;; Don't call continuation - return error directly
             (values nil clgrpc.grpc:+grpc-status-unauthenticated+ "Not authorized" nil))
           (handler (req ctx)
             (declare (ignore req ctx))
             (setf handler-called t)
             (values nil 0 nil nil)))

      (multiple-value-bind (response status msg meta)
          (clgrpc.server::execute-unary-interceptor-chain
           (list #'auth-interceptor)
           #'handler
           nil nil info)
        (declare (ignore response meta))
        (is (not handler-called))
        (is (= status clgrpc.grpc:+grpc-status-unauthenticated+))
        (is (string= msg "Not authorized"))))))

(test interceptor-can-modify-request
  "Test interceptor can modify request before handler"
  (let ((info (clgrpc.server::make-interceptor-info :service "svc" :method "m" :rpc-type :unary)))

    (flet ((uppercase-interceptor (req ctx info cont)
             (declare (ignore info))
             (let ((modified (map '(vector (unsigned-byte 8))
                                 (lambda (b) (if (<= 97 b 122) (- b 32) b))
                                 req)))
               (funcall cont modified ctx)))
           (handler (req ctx)
             (declare (ignore ctx))
             (values req 0 nil nil)))

      (multiple-value-bind (response status msg meta)
          (clgrpc.server::execute-unary-interceptor-chain
           (list #'uppercase-interceptor)
           #'handler
           (babel:string-to-octets "hello")
           nil info)
        (declare (ignore status msg meta))
        (is (equalp response (babel:string-to-octets "HELLO")))))))

(test metadata-validator-interceptor
  "Test metadata validator interceptor"
  (let ((validator (clgrpc.server::metadata-validator-interceptor '("authorization" "x-request-id")))
        (info (clgrpc.server::make-interceptor-info :service "svc" :method "m" :rpc-type :unary)))

    ;; Missing metadata - should fail
    (let ((ctx (clgrpc.server::make-handler-context :metadata '())))
      (multiple-value-bind (response status msg meta)
          (funcall validator nil ctx info (lambda (r c) (declare (ignore r c)) (values nil 0 nil nil)))
        (declare (ignore response meta))
        (is (= status clgrpc.grpc:+grpc-status-unauthenticated+))
        (is (search "authorization" msg))))

    ;; Partial metadata - should fail
    (let ((ctx (clgrpc.server::make-handler-context :metadata '(("authorization" . "token")))))
      (multiple-value-bind (response status msg meta)
          (funcall validator nil ctx info (lambda (r c) (declare (ignore r c)) (values nil 0 nil nil)))
        (declare (ignore response meta))
        (is (= status clgrpc.grpc:+grpc-status-unauthenticated+))
        (is (search "x-request-id" msg))))

    ;; Complete metadata - should succeed
    (let ((ctx (clgrpc.server::make-handler-context
                :metadata '(("authorization" . "token") ("x-request-id" . "123")))))
      (multiple-value-bind (response status msg meta)
          (funcall validator nil ctx info (lambda (r c) (declare (ignore r c)) (values nil 0 nil nil)))
        (declare (ignore response msg meta))
        (is (= status 0))))))

(test auth-interceptor
  "Test auth interceptor"
  (let ((auth (clgrpc.server::auth-interceptor
               (lambda (metadata)
                 (let ((token (cdr (assoc "authorization" metadata :test #'string-equal))))
                   (and token (string= token "Bearer secret"))))))
        (info (clgrpc.server::make-interceptor-info :service "svc" :method "m" :rpc-type :unary)))

    ;; No auth - should fail
    (let ((ctx (clgrpc.server::make-handler-context :metadata '())))
      (multiple-value-bind (response status msg meta)
          (funcall auth nil ctx info (lambda (r c) (declare (ignore r c)) (values nil 0 nil nil)))
        (declare (ignore response msg meta))
        (is (= status clgrpc.grpc:+grpc-status-unauthenticated+))))

    ;; Wrong token - should fail
    (let ((ctx (clgrpc.server::make-handler-context :metadata '(("authorization" . "Bearer wrong")))))
      (multiple-value-bind (response status msg meta)
          (funcall auth nil ctx info (lambda (r c) (declare (ignore r c)) (values nil 0 nil nil)))
        (declare (ignore response msg meta))
        (is (= status clgrpc.grpc:+grpc-status-unauthenticated+))))

    ;; Correct token - should succeed
    (let ((ctx (clgrpc.server::make-handler-context :metadata '(("authorization" . "Bearer secret")))))
      (multiple-value-bind (response status msg meta)
          (funcall auth nil ctx info (lambda (r c) (declare (ignore r c)) (values nil 0 nil nil)))
        (declare (ignore response msg meta))
        (is (= status 0))))))

;;; ============================================================
;;; Server Streaming Support Tests
;;; ============================================================

(test server-stream-queue-operations
  "Test server stream queue operations"
  (let ((queue (clgrpc.server::make-queue)))
    ;; Initially empty
    (is (clgrpc.server::queue-empty-p queue))

    ;; Push items
    (clgrpc.server::queue-push queue "first")
    (clgrpc.server::queue-push queue "second")
    (clgrpc.server::queue-push queue "third")

    ;; Not empty now
    (is (not (clgrpc.server::queue-empty-p queue)))

    ;; Pop in FIFO order
    (is (string= (clgrpc.server::queue-pop queue) "first"))
    (is (string= (clgrpc.server::queue-pop queue) "second"))
    (is (string= (clgrpc.server::queue-pop queue) "third"))

    ;; Empty again
    (is (clgrpc.server::queue-empty-p queue))
    (is (null (clgrpc.server::queue-pop queue)))))

;;; ============================================================
;;; Server Integration Tests
;;; ============================================================

(defvar *server-test-port-counter* (+ 52000 (random 1000))
  "Port counter for server tests")

(defun get-server-test-port ()
  (incf *server-test-port-counter*))

(test server-creation
  "Test creating a server"
  (let ((server (clgrpc.server:make-server :port 50099)))
    (is (not (null server)))
    (is (typep server 'clgrpc.server::grpc-server))))

(test server-start-stop
  "Test starting and stopping a server"
  (let* ((port (get-server-test-port))
         (server (clgrpc.server:make-server :port port)))
    ;; Start server
    (clgrpc.server:start-server server)
    (sleep 0.2)

    ;; Server should be running (check by trying to connect)
    (let ((socket nil))
      (unwind-protect
          (progn
            (setf socket (clgrpc.transport:make-tcp-connection "localhost" port :timeout 2))
            (is (not (null socket))))
        (when socket
          (ignore-errors (close socket)))))

    ;; Stop server
    (clgrpc.server:stop-server server :timeout 2)))

(test server-handles-unary-rpc
  "Test server handles unary RPC correctly"
  (let* ((port (get-server-test-port))
         (server (clgrpc.server:make-server :port port)))
    ;; Register echo handler
    (clgrpc.server:register-handler
     (clgrpc.server:grpc-server-router server)
     "test.Echo"
     "Unary"
     (clgrpc.server:lambda-handler
       (values clgrpc.server::request-bytes 0 nil nil))
     :rpc-type :unary)

    (clgrpc.server:start-server server)
    (sleep 0.3)

    (unwind-protect
        (let ((channel (clgrpc.client:make-channel (format nil "localhost:~D" port) :secure nil)))
          (unwind-protect
              (let* ((request (babel:string-to-octets "Hello Server"))
                     (response (clgrpc.client:call-unary channel "test.Echo" "Unary" request)))
                (is (equalp response request)))
            (clgrpc.client:close-channel channel)))
      (clgrpc.server:stop-server server :timeout 5))))

(test server-handles-multiple-concurrent-rpcs
  "Test server handles multiple concurrent RPCs"
  (let* ((port (get-server-test-port))
         (server (clgrpc.server:make-server :port port)))
    ;; Register echo handler
    (clgrpc.server:register-handler
     (clgrpc.server:grpc-server-router server)
     "test.Echo"
     "Unary"
     (clgrpc.server:lambda-handler
       (values clgrpc.server::request-bytes 0 nil nil))
     :rpc-type :unary)

    (clgrpc.server:start-server server)
    (sleep 0.3)

    (unwind-protect
        (let ((channel (clgrpc.client:make-channel (format nil "localhost:~D" port) :secure nil))
              (results (make-array 5 :initial-element nil))
              (threads nil))
          (unwind-protect
              (progn
                ;; Launch 5 concurrent calls
                (dotimes (i 5)
                  (let ((idx i))
                    (push
                     (bt:make-thread
                      (lambda ()
                        (handler-case
                            (let* ((msg (format nil "Message ~D" idx))
                                   (request (babel:string-to-octets msg))
                                   (response (clgrpc.client:call-unary channel "test.Echo" "Unary" request)))
                              (setf (aref results idx) (equalp response request)))
                          (error () (setf (aref results idx) nil)))))
                     threads)))

                ;; Wait for all
                (dolist (thread threads)
                  (bt:join-thread thread))

                ;; All should succeed
                (dotimes (i 5)
                  (is (aref results i))))
            (clgrpc.client:close-channel channel)))
      (clgrpc.server:stop-server server :timeout 5))))

(test server-returns-unimplemented-for-unknown-method
  "Test server returns UNIMPLEMENTED for unknown methods"
  (let* ((port (get-server-test-port))
         (server (clgrpc.server:make-server :port port)))
    (clgrpc.server:start-server server)
    (sleep 0.3)

    (unwind-protect
        (let ((channel (clgrpc.client:make-channel (format nil "localhost:~D" port) :secure nil)))
          (unwind-protect
              (handler-case
                  (progn
                    (clgrpc.client:call-unary channel "nonexistent.Service" "Method"
                                             (babel:string-to-octets "test"))
                    (fail "Should have raised an error"))
                (clgrpc.grpc:grpc-error (e)
                  (is (= (clgrpc.grpc:grpc-error-status-code e) clgrpc.grpc:+grpc-status-unimplemented+))))
            (clgrpc.client:close-channel channel)))
      (clgrpc.server:stop-server server :timeout 5))))

(test server-graceful-shutdown
  "Test server graceful shutdown waits for active requests"
  (let* ((port (get-server-test-port))
         (server (clgrpc.server:make-server :port port))
         (handler-started nil)
         (handler-finished nil))
    ;; Register slow handler
    (clgrpc.server:register-handler
     (clgrpc.server:grpc-server-router server)
     "test.Echo"
     "Slow"
     (clgrpc.server:lambda-handler
       (setf handler-started t)
       (sleep 1)  ; Simulate slow operation
       (setf handler-finished t)
       (values clgrpc.server::request-bytes 0 nil nil))
     :rpc-type :unary)

    (clgrpc.server:start-server server)
    (sleep 0.3)

    (let ((channel (clgrpc.client:make-channel (format nil "localhost:~D" port) :secure nil)))
      (unwind-protect
          (progn
            ;; Start slow request in background
            (bt:make-thread
             (lambda ()
               (ignore-errors
                 (clgrpc.client:call-unary channel "test.Echo" "Slow"
                                          (babel:string-to-octets "test")))))

            ;; Wait for handler to start
            (loop until handler-started do (sleep 0.1))

            ;; Initiate shutdown - should wait for handler
            (clgrpc.server:stop-server server :timeout 5)

            ;; Handler should have completed
            (is-true handler-finished))
        (clgrpc.client:close-channel channel)))))

(test server-with-interceptors
  "Test server with interceptors"
  (let* ((port (get-server-test-port))
         (interceptor-called nil)
         (server (clgrpc.server:make-server
                  :port port
                  :unary-interceptors
                  (list (lambda (req ctx info cont)
                          (declare (ignore info))
                          (setf interceptor-called t)
                          (funcall cont req ctx))))))

    ;; Register handler
    (clgrpc.server:register-handler
     (clgrpc.server:grpc-server-router server)
     "test.Echo"
     "Unary"
     (clgrpc.server:lambda-handler
       (values clgrpc.server::request-bytes 0 nil nil))
     :rpc-type :unary)

    (clgrpc.server:start-server server)
    (sleep 0.3)

    (unwind-protect
        (let ((channel (clgrpc.client:make-channel (format nil "localhost:~D" port) :secure nil)))
          (unwind-protect
              (progn
                (clgrpc.client:call-unary channel "test.Echo" "Unary"
                                         (babel:string-to-octets "test"))
                (is-true interceptor-called))
            (clgrpc.client:close-channel channel)))
      (clgrpc.server:stop-server server :timeout 5))))
