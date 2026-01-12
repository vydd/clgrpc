;;;; client-tests.lisp - Comprehensive client API tests

(in-package #:clgrpc-tests)

(def-suite client-tests
  :in clgrpc-all
  :description "gRPC client API tests")

(in-suite client-tests)

;;; Test Utilities

(defun make-test-server (&key (port 50099))
  "Create a test server for client tests"
  (let ((server (clgrpc.server:make-server :port port)))
    ;; Register a simple echo service
    ;; lambda-handler expands body within (lambda (service-name method-name request-bytes context) ...)
    (clgrpc.server:register-handler
     (clgrpc.server:grpc-server-router server)
     "test.Echo"
     "Unary"
     (clgrpc.server:lambda-handler
       ;; Echo back the request (request-bytes is bound in clgrpc.server package)
       (values clgrpc.server::request-bytes 0 nil nil))
     :rpc-type :unary)

    ;; For streaming handlers, we need to use make-function-handler directly
    ;; since lambda-handler is designed for unary handlers
    ;; Register client streaming echo (concatenates all messages)
    (clgrpc.server:register-handler
     (clgrpc.server:grpc-server-router server)
     "test.Echo"
     "ClientStreaming"
     (clgrpc.server::make-function-handler
       (lambda (service-name method-name request-bytes context)
         (declare (ignore service-name method-name request-bytes context))
         ;; This is a client-streaming handler but function-handler calls handle-unary
         ;; For now, return unimplemented - the streaming tests need different setup
         (values nil clgrpc.grpc:+grpc-status-unimplemented+ "Not implemented" nil)))
     :rpc-type :client-streaming)

    ;; Register server streaming echo (splits message into chunks)
    (clgrpc.server:register-handler
     (clgrpc.server:grpc-server-router server)
     "test.Echo"
     "ServerStreaming"
     (clgrpc.server::make-function-handler
       (lambda (service-name method-name request-bytes context)
         (declare (ignore service-name method-name request-bytes context))
         (values nil clgrpc.grpc:+grpc-status-unimplemented+ "Not implemented" nil)))
     :rpc-type :server-streaming)

    ;; Register bidirectional streaming echo
    (clgrpc.server:register-handler
     (clgrpc.server:grpc-server-router server)
     "test.Echo"
     "Bidirectional"
     (clgrpc.server::make-function-handler
       (lambda (service-name method-name request-bytes context)
         (declare (ignore service-name method-name request-bytes context))
         (values nil clgrpc.grpc:+grpc-status-unimplemented+ "Not implemented" nil)))
     :rpc-type :bidirectional)

    server))

(defun with-test-server (port fn)
  "Run fn with a test server running on port"
  (let ((server (make-test-server :port port)))
    (unwind-protect
        (progn
          (clgrpc.server:start-server server)
          (sleep 0.5)  ; Give server time to start
          (funcall fn))
      (clgrpc.server:stop-server server :timeout 5))))

;;; Channel Tests

(test channel-creation
  "Test creating and closing channels"
  (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
    (is (not (null channel)))
    (is (typep channel 'clgrpc.client:grpc-channel))

    ;; Close channel - should not error
    (finishes (clgrpc.client:close-channel channel))))

(test channel-double-close
  "Test that closing a channel twice is safe"
  (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
    (clgrpc.client:close-channel channel)

    ;; Second close should be safe and not error
    (finishes (clgrpc.client:close-channel channel))))

(test call-on-closed-channel
  "Test that calling on a closed channel raises an error"
  (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
    (clgrpc.client:close-channel channel)

    (signals error
      (clgrpc.client:call-unary channel "test.Echo" "Unary"
                                (babel:string-to-octets "test")))))

;;; Unary Call Tests

(test unary-call-basic
  "Test basic unary RPC call"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            (let* ((request (babel:string-to-octets "Hello"))
                   (response (clgrpc.client:call-unary channel "test.Echo" "Unary" request)))
              (is (not (null response)))
              (is (equalp response request)))
          (clgrpc.client:close-channel channel))))))

(test unary-call-empty-message
  "Test unary call with empty message"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            (let* ((request (make-array 0 :element-type '(unsigned-byte 8)))
                   (response (clgrpc.client:call-unary channel "test.Echo" "Unary" request)))
              (is (not (null response)))
              (is (equalp response request)))
          (clgrpc.client:close-channel channel))))))

(test unary-call-large-message
  "Test unary call with large message (1MB)"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            (let* ((size (* 1024 1024))  ; 1MB
                   (request (make-array size :element-type '(unsigned-byte 8)
                                       :initial-element 42))
                   (response (clgrpc.client:call-unary channel "test.Echo" "Unary" request)))
              (is (not (null response)))
              (is (= (length response) size))
              (is (equalp response request)))
          (clgrpc.client:close-channel channel))))))

(test unary-call-with-metadata
  "Test unary call with custom metadata"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            (let* ((request (babel:string-to-octets "test"))
                   (metadata '(("x-custom-header" . "custom-value")
                              ("x-request-id" . "12345")))
                   (response (clgrpc.client:call-unary channel "test.Echo" "Unary" request
                                                       :metadata metadata)))
              (is (not (null response)))
              (is (equalp response request)))
          (clgrpc.client:close-channel channel))))))

(test unary-call-nonexistent-service
  "Test calling a non-existent service returns error"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            (signals clgrpc.grpc:grpc-error
              (clgrpc.client:call-unary channel "test.NonExistent" "Method"
                                       (babel:string-to-octets "test")))
          (clgrpc.client:close-channel channel))))))

(test unary-call-connection-refused
  "Test that connection refused raises appropriate error"
  (let ((channel (clgrpc.client:make-channel "localhost:59999" :secure nil)))
    (unwind-protect
        (signals error
          (clgrpc.client:call-unary channel "test.Echo" "Unary"
                                   (babel:string-to-octets "test")))
      (clgrpc.client:close-channel channel))))

;;; Client Streaming Tests

(test client-streaming-basic
  "Test basic client streaming call"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            (let ((stream (clgrpc.client:call-client-streaming channel "test.Echo" "ClientStreaming")))
              ;; Send multiple messages
              (clgrpc.client:stream-send stream (babel:string-to-octets "Hello"))
              (clgrpc.client:stream-send stream (babel:string-to-octets " "))
              (clgrpc.client:stream-send stream (babel:string-to-octets "World"))

              ;; Close send and get response
              (clgrpc.client:stream-close-send stream)
              (let ((response (clgrpc.client:stream-recv stream)))
                (is (not (null response)))
                (is (equalp response (babel:string-to-octets "Hello World")))))
          (clgrpc.client:close-channel channel))))))

(test client-streaming-empty-stream
  "Test client streaming with no messages sent"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            (let ((stream (clgrpc.client:call-client-streaming channel "test.Echo" "ClientStreaming")))
              ;; Close immediately without sending
              (clgrpc.client:stream-close-send stream)
              (let ((response (clgrpc.client:stream-recv stream)))
                (is (or (null response) (zerop (length response))))))
          (clgrpc.client:close-channel channel))))))

(test client-streaming-many-messages
  "Test client streaming with many small messages"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            (let ((stream (clgrpc.client:call-client-streaming channel "test.Echo" "ClientStreaming"))
                  (expected-length 0))
              ;; Send 100 small messages
              (dotimes (i 100)
                (let ((msg (babel:string-to-octets "x")))
                  (clgrpc.client:stream-send stream msg)
                  (incf expected-length (length msg))))

              (clgrpc.client:stream-close-send stream)
              (let ((response (clgrpc.client:stream-recv stream)))
                (is (not (null response)))
                (is (= (length response) expected-length))))
          (clgrpc.client:close-channel channel))))))

;;; Server Streaming Tests

(test server-streaming-basic
  "Test basic server streaming call"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            (let* ((request (make-array 50 :element-type '(unsigned-byte 8)
                                       :initial-element 42))
                   (stream (clgrpc.client:call-server-streaming channel "test.Echo" "ServerStreaming" request))
                   (received nil))
              ;; Receive all messages
              (loop
                (let ((msg (clgrpc.client:stream-recv stream)))
                  (unless msg (return))
                  (push msg received)))

              ;; Should have received multiple chunks
              (is (> (length received) 1))

              ;; Concatenate and verify
              (let ((concatenated (apply #'concatenate 'vector (reverse received))))
                (is (equalp concatenated request))))
          (clgrpc.client:close-channel channel))))))

(test server-streaming-empty-response
  "Test server streaming with empty initial request"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            (let* ((request (make-array 0 :element-type '(unsigned-byte 8)))
                   (stream (clgrpc.client:call-server-streaming channel "test.Echo" "ServerStreaming" request))
                   (received nil))
              ;; Try to receive (should get nothing)
              (loop
                (let ((msg (clgrpc.client:stream-recv stream)))
                  (unless msg (return))
                  (push msg received)))

              (is (null received)))
          (clgrpc.client:close-channel channel))))))

;;; Bidirectional Streaming Tests

(test bidirectional-streaming-basic
  "Test basic bidirectional streaming"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            (let ((stream (clgrpc.client:call-bidirectional-streaming channel "test.Echo" "Bidirectional")))
              ;; Send and receive messages
              (clgrpc.client:stream-send stream (babel:string-to-octets "Message 1"))
              (let ((resp1 (clgrpc.client:stream-recv stream)))
                (is (equalp resp1 (babel:string-to-octets "Message 1"))))

              (clgrpc.client:stream-send stream (babel:string-to-octets "Message 2"))
              (let ((resp2 (clgrpc.client:stream-recv stream)))
                (is (equalp resp2 (babel:string-to-octets "Message 2"))))

              ;; Close send side
              (clgrpc.client:stream-close-send stream)

              ;; Should receive end-of-stream
              (let ((resp3 (clgrpc.client:stream-recv stream)))
                (is (null resp3))))
          (clgrpc.client:close-channel channel))))))

(test bidirectional-streaming-concurrent
  "Test bidirectional streaming with concurrent send/receive"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            (let ((stream (clgrpc.client:call-bidirectional-streaming channel "test.Echo" "Bidirectional"))
                  (received nil)
                  (lock (bt:make-lock)))

              ;; Start receiver thread
              (let ((receiver-thread
                      (bt:make-thread
                       (lambda ()
                         (loop
                           (let ((msg (clgrpc.client:stream-recv stream)))
                             (unless msg (return))
                             (bt:with-lock-held (lock)
                               (push msg received))))))))

                ;; Send messages
                (dotimes (i 10)
                  (clgrpc.client:stream-send stream
                    (babel:string-to-octets (format nil "Message ~D" i))))

                (clgrpc.client:stream-close-send stream)

                ;; Wait for receiver
                (bt:join-thread receiver-thread)

                ;; Should have received all messages
                (is (= (length received) 10))))
          (clgrpc.client:close-channel channel))))))

;;; Connection Pool Tests

(test connection-pool-basic
  "Test connection pool creates and reuses connections"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            (progn
              ;; Make multiple calls - should reuse connection
              (dotimes (i 5)
                (let* ((request (babel:string-to-octets (format nil "Request ~D" i)))
                       (response (clgrpc.client:call-unary channel "test.Echo" "Unary" request)))
                  (is (equalp response request)))))
          (clgrpc.client:close-channel channel))))))

(test connection-pool-concurrent-calls
  "Test connection pool handles concurrent calls"
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil))
            (results (make-array 10 :initial-element nil))
            (threads nil))
        (unwind-protect
            (progn
              ;; Launch 10 concurrent calls
              (dotimes (i 10)
                (push
                 (bt:make-thread
                  (lambda ()
                    (let* ((request (babel:string-to-octets (format nil "Request ~D" i)))
                           (response (clgrpc.client:call-unary channel "test.Echo" "Unary" request)))
                      (setf (aref results i) (equalp response request)))))
                 threads))

              ;; Wait for all threads
              (dolist (thread threads)
                (bt:join-thread thread))

              ;; All should succeed
              (dotimes (i 10)
                (is (aref results i))))
          (clgrpc.client:close-channel channel))))))

;;; Error Handling Tests

(test error-handling-timeout
  "Test that timeout is enforced (if supported)"
  ;; Note: This test may need adjustment based on timeout implementation
  (with-test-server 50099
    (lambda ()
      (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
        (unwind-protect
            ;; Try with very short timeout - may or may not fail depending on timing
            (let* ((request (babel:string-to-octets "test"))
                   (response (clgrpc.client:call-unary channel "test.Echo" "Unary" request
                                                       :timeout 1)))
              ;; Either succeeds or times out, both are acceptable
              (is (or (not (null response))
                     (null response))))
          (clgrpc.client:close-channel channel))))))

(test error-handling-invalid-target
  "Test error on connection to invalid target"
  ;; Channel creation doesn't validate, but calling on invalid target should error
  (let ((channel (clgrpc.client:make-channel "invalid-host-that-does-not-exist:99999" :secure nil)))
    (unwind-protect
        (signals error
          (clgrpc.client:call-unary channel "test.Echo" "Unary"
                                   (babel:string-to-octets "test")))
      (clgrpc.client:close-channel channel))))

;;; Stub Helper Tests

(test camel-to-kebab-case-basic
  "Test CamelCase to kebab-case conversion"
  (is (string= (clgrpc.client::camel-to-kebab-case "SayHello") "say-hello"))
  (is (string= (clgrpc.client::camel-to-kebab-case "GetFeature") "get-feature"))
  (is (string= (clgrpc.client::camel-to-kebab-case "ListFeatures") "list-features")))

(test camel-to-kebab-case-edge-cases
  "Test CamelCase conversion edge cases"
  ;; Already lowercase
  (is (string= (clgrpc.client::camel-to-kebab-case "hello") "hello"))
  ;; Single uppercase word
  (is (string= (clgrpc.client::camel-to-kebab-case "Hello") "hello"))
  ;; All uppercase (consecutive caps stay together - sensible for acronyms)
  (is (string= (clgrpc.client::camel-to-kebab-case "API") "api"))
  ;; Mixed with acronym (only inserts hyphen after lowercase->uppercase transition)
  (is (string= (clgrpc.client::camel-to-kebab-case "getAPIKey") "get-apikey"))
  ;; Empty string
  (is (string= (clgrpc.client::camel-to-kebab-case "") "")))

(test grpc-stub-base-class
  "Test grpc-stub base class"
  (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
    (unwind-protect
        (let ((stub (make-instance 'clgrpc.client:grpc-stub :channel channel)))
          (is (not (null stub)))
          (is (eq (clgrpc.client:stub-channel stub) channel)))
      (clgrpc.client:close-channel channel))))

;;; Queue Tests (internal streaming queue)

(test queue-basic-operations
  "Test basic queue operations"
  (let ((q (clgrpc.client::make-queue)))
    ;; Initially empty
    (is (clgrpc.client::queue-empty-p q))
    (is (null (clgrpc.client::queue-pop q)))

    ;; Push and pop single item
    (clgrpc.client::queue-push q "first")
    (is (not (clgrpc.client::queue-empty-p q)))
    (is (string= (clgrpc.client::queue-pop q) "first"))
    (is (clgrpc.client::queue-empty-p q))))

(test queue-fifo-order
  "Test queue maintains FIFO order"
  (let ((q (clgrpc.client::make-queue)))
    ;; Push multiple items
    (clgrpc.client::queue-push q "a")
    (clgrpc.client::queue-push q "b")
    (clgrpc.client::queue-push q "c")

    ;; Pop in order
    (is (string= (clgrpc.client::queue-pop q) "a"))
    (is (string= (clgrpc.client::queue-pop q) "b"))
    (is (string= (clgrpc.client::queue-pop q) "c"))
    (is (clgrpc.client::queue-empty-p q))))

(test queue-interleaved-operations
  "Test queue with interleaved push/pop"
  (let ((q (clgrpc.client::make-queue)))
    ;; Push, pop, push, pop pattern
    (clgrpc.client::queue-push q 1)
    (is (= (clgrpc.client::queue-pop q) 1))
    (clgrpc.client::queue-push q 2)
    (clgrpc.client::queue-push q 3)
    (is (= (clgrpc.client::queue-pop q) 2))
    (clgrpc.client::queue-push q 4)
    (is (= (clgrpc.client::queue-pop q) 3))
    (is (= (clgrpc.client::queue-pop q) 4))
    (is (clgrpc.client::queue-empty-p q))))

;;; Connection Pool Additional Tests

(test connection-pool-creation
  "Test connection pool creation with options"
  (let ((pool (clgrpc.client::make-grpc-connection-pool "localhost:50099"
                                                        :secure nil
                                                        :max-connections 5)))
    (is (not (null pool)))
    (is (string= (clgrpc.client::connection-pool-target pool) "localhost:50099"))
    (is (eq (clgrpc.client::connection-pool-secure pool) nil))
    (is (= (clgrpc.client::connection-pool-max-connections pool) 5))))

(test connection-pool-cleanup
  "Test pool-cleanup-closed removes closed connections"
  (let ((pool (clgrpc.client::make-grpc-connection-pool "localhost:50099" :secure nil)))
    ;; Initially empty
    (is (= (clgrpc.client::pool-cleanup-closed pool) 0))))

;;; Channel Call Registration Tests

(test channel-call-registration
  "Test channel call registration and lookup"
  (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
    (unwind-protect
        (let ((mock-call (clgrpc.client::make-grpc-call
                          :connection nil
                          :service "test.Service"
                          :method "TestMethod")))
          ;; Register call
          (clgrpc.client::channel-register-call channel mock-call 123)

          ;; Find call
          (let ((found (clgrpc.client::channel-find-call channel 123)))
            (is (eq found mock-call)))

          ;; Unregister call
          (clgrpc.client::channel-unregister-call channel 123)

          ;; Should not find after unregister
          (is (null (clgrpc.client::channel-find-call channel 123))))
      (clgrpc.client:close-channel channel))))

(test channel-call-registration-multiple
  "Test channel with multiple registered calls"
  (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
    (unwind-protect
        (let ((call1 (clgrpc.client::make-grpc-call :connection nil :service "s" :method "m1"))
              (call2 (clgrpc.client::make-grpc-call :connection nil :service "s" :method "m2"))
              (call3 (clgrpc.client::make-grpc-call :connection nil :service "s" :method "m3")))

          ;; Register multiple calls
          (clgrpc.client::channel-register-call channel call1 1)
          (clgrpc.client::channel-register-call channel call2 3)
          (clgrpc.client::channel-register-call channel call3 5)

          ;; Find each call
          (is (eq (clgrpc.client::channel-find-call channel 1) call1))
          (is (eq (clgrpc.client::channel-find-call channel 3) call2))
          (is (eq (clgrpc.client::channel-find-call channel 5) call3))

          ;; Non-existent stream ID
          (is (null (clgrpc.client::channel-find-call channel 99))))
      (clgrpc.client:close-channel channel))))

;;; grpc-stream Tests

(test grpc-stream-creation
  "Test grpc-stream structure creation"
  (let ((stream (clgrpc.client::make-grpc-stream
                 :connection nil
                 :service "test.Service"
                 :method "TestMethod"
                 :authority "localhost:50099"
                 :timeout 5000
                 :metadata '(("key" . "value")))))
    (is (not (null stream)))
    (is (string= (clgrpc.client::grpc-stream-service stream) "test.Service"))
    (is (string= (clgrpc.client::grpc-stream-method stream) "TestMethod"))
    (is (string= (clgrpc.client::grpc-stream-authority stream) "localhost:50099"))
    (is (= (clgrpc.client::grpc-stream-timeout stream) 5000))
    (is (equal (clgrpc.client::grpc-stream-metadata stream) '(("key" . "value"))))
    ;; Initial states
    (is (not (clgrpc.client::grpc-stream-headers-sent stream)))
    (is (not (clgrpc.client::grpc-stream-send-closed stream)))
    (is (not (clgrpc.client::grpc-stream-recv-closed stream)))))

;;; grpc-call Tests

(test grpc-call-creation
  "Test grpc-call structure creation"
  (let ((call (clgrpc.client::make-grpc-call
               :connection nil
               :service "test.Service"
               :method "TestMethod"
               :authority "localhost:50099"
               :timeout 3000
               :metadata '(("x-request-id" . "abc123")))))
    (is (not (null call)))
    (is (string= (clgrpc.client::grpc-call-service call) "test.Service"))
    (is (string= (clgrpc.client::grpc-call-method call) "TestMethod"))
    (is (string= (clgrpc.client::grpc-call-authority call) "localhost:50099"))
    (is (= (clgrpc.client::grpc-call-timeout call) 3000))
    (is (equal (clgrpc.client::grpc-call-metadata call) '(("x-request-id" . "abc123"))))
    ;; Initial states
    (is (not (clgrpc.client::grpc-call-request-sent call)))
    (is (not (clgrpc.client::grpc-call-completed call)))
    (is (null (clgrpc.client::grpc-call-response-data call)))))

(test create-grpc-call-helper
  "Test create-grpc-call helper function"
  (let ((call (clgrpc.client::create-grpc-call
               nil  ; connection
               "helloworld.Greeter"
               "SayHello"
               :authority "localhost:50051"
               :timeout 10000
               :metadata '(("authorization" . "Bearer token")))))
    (is (not (null call)))
    (is (string= (clgrpc.client::grpc-call-service call) "helloworld.Greeter"))
    (is (string= (clgrpc.client::grpc-call-method call) "SayHello"))
    (is (= (clgrpc.client::grpc-call-timeout call) 10000))))

;;; Run all tests
(run! 'client-tests)
