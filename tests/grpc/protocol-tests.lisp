;;;; protocol-tests.lisp - gRPC protocol tests

(in-package #:clgrpc-tests)

(in-suite grpc-tests)

;;; Status Code Tests

(test status-code-validation
  "Test status code validation"
  (is (valid-status-code-p 0))
  (is (valid-status-code-p 16))
  (is (not (valid-status-code-p 17)))
  (is (not (valid-status-code-p -1))))

(test status-code-names
  "Test status code name mapping"
  (is (string= (status-code-name 0) "OK"))
  (is (string= (status-code-name 1) "CANCELLED"))
  (is (string= (status-code-name 13) "INTERNAL"))
  (is (string= (status-code-name 16) "UNAUTHENTICATED"))
  (is (string= (status-code-name 99) "UNKNOWN")))

(test retryable-status-codes
  "Test retryable status detection"
  (is (retryable-status-p 1))   ; CANCELLED
  (is (retryable-status-p 14))  ; UNAVAILABLE
  (is (not (retryable-status-p 0)))   ; OK
  (is (not (retryable-status-p 3)))   ; INVALID_ARGUMENT
  (is (not (retryable-status-p 13)))) ; INTERNAL

;;; Error Condition Tests

(test grpc-error-creation
  "Test creating gRPC errors"
  (let ((err (make-grpc-error 13 "test error" "details")))
    (is (= (grpc-error-status-code err) 13))
    (is (string= (grpc-error-message err) "test error"))
    (is (string= (grpc-error-details err) "details"))))

(test grpc-error-signaling
  "Test signaling gRPC errors"
  (signals grpc-error
    (signal-grpc-error 13 "internal error"))
  (signals grpc-internal
    (signal-grpc-internal "internal error"))
  (signals grpc-not-found
    (signal-grpc-not-found "not found"))
  (signals grpc-invalid-argument
    (signal-grpc-invalid-argument "invalid")))

;;; Message Framing Tests

(test message-frame-encode-basic
  "Test basic gRPC message encoding"
  (let* ((message (bytes 1 2 3 4 5))
         (framed (encode-grpc-message message)))
    ;; Check framed message structure
    (is (= (length framed) 10))  ; 5 header + 5 message
    (is (= (aref framed 0) 0))  ; Not compressed
    ;; Length should be 5 (big-endian)
    (is (= (aref framed 1) 0))
    (is (= (aref framed 2) 0))
    (is (= (aref framed 3) 0))
    (is (= (aref framed 4) 5))
    ;; Message payload
    (is (equalp (subseq framed 5) message))))

(test message-frame-encode-compressed
  "Test gRPC message encoding with compression flag"
  (let* ((message (bytes 1 2 3))
         (framed (encode-grpc-message message :compressed t)))
    (is (= (aref framed 0) 1))))  ; Compressed flag

(test message-frame-decode-basic
  "Test basic gRPC message decoding"
  (let* ((message (bytes 1 2 3 4 5))
         (framed (encode-grpc-message message)))
    (multiple-value-bind (decoded-msg compressed bytes-read)
        (decode-grpc-message framed)
      (is (equalp decoded-msg message))
      (is (= compressed 0))
      (is (= bytes-read 10)))))

(test message-frame-round-trip
  "Test message frame encoding/decoding round-trip"
  (let ((messages (list (bytes) ; Empty message
                        (bytes 42) ; Single byte
                        (bytes 1 2 3 4 5 6 7 8 9 10)))) ; Multiple bytes
    (dolist (msg messages)
      (let ((framed (encode-grpc-message msg)))
        (multiple-value-bind (decoded compressed bytes-read)
            (decode-grpc-message framed)
          (declare (ignore compressed bytes-read))
          (is (equalp decoded msg)))))))

(test message-frame-multiple
  "Test splitting and joining multiple messages"
  (let* ((msg1 (bytes 1 2 3))
         (msg2 (bytes 4 5))
         (msg3 (bytes 6 7 8 9))
         (messages (list msg1 msg2 msg3))
         (joined (join-grpc-messages messages))
         (split (split-grpc-messages joined)))
    (is (= (length split) 3))
    (is (equalp (first split) msg1))
    (is (equalp (second split) msg2))
    (is (equalp (third split) msg3))))

;;; Metadata Tests

(test metadata-key-binary-detection
  "Test detection of binary metadata keys"
  (is (not (metadata-key-binary-p "content-type")))
  (is (not (metadata-key-binary-p :method)))
  (is (metadata-key-binary-p "custom-bin"))
  (is (metadata-key-binary-p :trace-bin)))

(test metadata-encode-decode-basic
  "Test basic metadata encoding/decoding"
  (let* ((metadata '((:key1 . "value1")
                     (:key2 . "value2")))
         (encoded (encode-metadata metadata))
         (decoded (decode-metadata encoded)))
    (is (= (length encoded) 2))
    (is (assoc "key1" encoded :test #'string=))
    (is (assoc "key2" encoded :test #'string=))))

;;; Timeout Tests

(test timeout-encode-milliseconds
  "Test encoding timeout in milliseconds"
  (is (string= (encode-grpc-timeout 500) "500m"))
  (is (string= (encode-grpc-timeout 999) "999m")))

(test timeout-encode-seconds
  "Test encoding timeout in seconds"
  (is (string= (encode-grpc-timeout 1000) "1S"))
  (is (string= (encode-grpc-timeout 5000) "5S"))
  (is (string= (encode-grpc-timeout 59999) "59S")))

(test timeout-encode-minutes
  "Test encoding timeout in minutes"
  (is (string= (encode-grpc-timeout 60000) "1M"))
  (is (string= (encode-grpc-timeout 300000) "5M")))

(test timeout-encode-hours
  "Test encoding timeout in hours"
  (is (string= (encode-grpc-timeout 3600000) "1H"))
  (is (string= (encode-grpc-timeout 7200000) "2H")))

(test timeout-decode-units
  "Test decoding various timeout units"
  (is (= (decode-grpc-timeout "100m") 100))
  (is (= (decode-grpc-timeout "10S") 10000))
  (is (= (decode-grpc-timeout "5M") 300000))
  (is (= (decode-grpc-timeout "1H") 3600000)))

(test timeout-round-trip
  "Test timeout encoding/decoding round-trip"
  (let ((timeouts '(100 1000 60000 3600000)))
    (dolist (timeout timeouts)
      (let* ((encoded (encode-grpc-timeout timeout))
             (decoded (decode-grpc-timeout encoded)))
        ;; Allow some rounding due to unit conversion
        (is (< (abs (- decoded timeout)) timeout))))))

;;; Request Headers Tests

(test request-headers-basic
  "Test basic gRPC request header encoding"
  (let ((headers (encode-grpc-request-headers "helloworld.Greeter" "SayHello"
                                              :authority "localhost:50051")))
    (is (assoc ":method" headers :test #'string=))
    (is (string= (cdr (assoc ":method" headers :test #'string=)) "POST"))
    (is (assoc ":path" headers :test #'string=))
    (is (string= (cdr (assoc ":path" headers :test #'string=)) "/helloworld.Greeter/SayHello"))
    (is (assoc ":authority" headers :test #'string=))
    (is (assoc "content-type" headers :test #'string=))
    (is (string= (cdr (assoc "content-type" headers :test #'string=)) "application/grpc+proto"))
    (is (assoc "te" headers :test #'string=))))

(test request-headers-with-timeout
  "Test gRPC request headers with timeout"
  (let ((headers (encode-grpc-request-headers "service" "method"
                                              :timeout 5000)))
    (is (assoc "grpc-timeout" headers :test #'string=))
    (is (string= (cdr (assoc "grpc-timeout" headers :test #'string=)) "5S"))))

(test request-headers-with-metadata
  "Test gRPC request headers with custom metadata"
  (let ((headers (encode-grpc-request-headers "service" "method"
                                              :metadata '((:custom . "value")))))
    (is (assoc "custom" headers :test #'string=))))

;;; Response Headers Tests

(test response-headers-basic
  "Test basic gRPC response header encoding"
  (let ((headers (encode-grpc-response-headers)))
    (is (assoc ":status" headers :test #'string=))
    (is (string= (cdr (assoc ":status" headers :test #'string=)) "200"))
    (is (assoc "content-type" headers :test #'string=))))

;;; Trailers Tests

(test trailers-encode-status-only
  "Test encoding trailers with status only"
  (let ((trailers (encode-grpc-trailers 0)))
    (is (assoc "grpc-status" trailers :test #'string=))
    (is (string= (cdr (assoc "grpc-status" trailers :test #'string=)) "0"))
    (is (not (assoc "grpc-message" trailers :test #'string=)))))

(test trailers-encode-status-and-message
  "Test encoding trailers with status and message"
  (let ((trailers (encode-grpc-trailers 13 "Internal error")))
    (is (assoc "grpc-status" trailers :test #'string=))
    (is (string= (cdr (assoc "grpc-status" trailers :test #'string=)) "13"))
    (is (assoc "grpc-message" trailers :test #'string=))
    (is (string= (cdr (assoc "grpc-message" trailers :test #'string=)) "Internal error"))))

(test trailers-decode
  "Test decoding trailers"
  (let ((trailers '(("grpc-status" . "0")
                    ("grpc-message" . "OK"))))
    (multiple-value-bind (status message)
        (decode-grpc-trailers trailers)
      (is (= status 0))
      (is (string= message "OK")))))

(test trailers-decode-without-message
  "Test decoding trailers without message"
  (let ((trailers '(("grpc-status" . "5"))))
    (multiple-value-bind (status message)
        (decode-grpc-trailers trailers)
      (is (= status 5))
      (is (string= message "")))))

;;; Content Type Tests

(test content-type-validation
  "Test gRPC content type validation"
  (is (grpc-content-type-p "application/grpc"))
  (is (grpc-content-type-p "application/grpc+proto"))
  (is (grpc-content-type-p "application/grpc+json"))
  (is (not (grpc-content-type-p "application/json")))
  (is (not (grpc-content-type-p "text/html"))))

;;; Message Serialization Tests

(test message-serialize-bytes
  "Test serializing byte array message"
  (let* ((msg (bytes 1 2 3 4 5))
         (serialized (serialize-message msg)))
    (is (equalp serialized msg))))

(test message-serialize-string
  "Test serializing string message"
  (let* ((msg "Hello, gRPC!")
         (serialized (serialize-message msg)))
    (is (typep serialized '(simple-array (unsigned-byte 8) (*))))))

(test message-deserialize-bytes
  "Test deserializing to bytes"
  (let* ((bytes (make-byte-array 5))
         (deserialized (deserialize-message bytes :bytes)))
    (is (equalp deserialized bytes))))

(test message-deserialize-string
  "Test deserializing to string"
  (let* ((text "Hello, gRPC!")
         (bytes (babel:string-to-octets text :encoding :utf-8))
         (deserialized (deserialize-message bytes :string)))
    (is (string= deserialized text))))

(test message-simple-text-round-trip
  "Test simple-text-message round-trip"
  (let* ((original (make-simple-text-message "Test message"))
         (serialized (serialize-message original))
         (deserialized (deserialize-message serialized 'simple-text-message)))
    (is (string= (message-text original) (message-text deserialized)))))

(test message-encode-decode-full-round-trip
  "Test full message encode/decode with framing"
  (let* ((message (bytes 72 101 108 108 111)) ; "Hello"
         (encoded (encode-grpc-message message))
         (decoded (multiple-value-bind (msg comp bytes)
                      (decode-grpc-message encoded)
                    (declare (ignore comp bytes))
                    msg)))
    (is (equalp decoded message))))
