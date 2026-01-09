;;;; call.lisp - Client call implementation
;;;
;;; Manages lifecycle of a single gRPC unary RPC call over HTTP/2

(in-package #:clgrpc.client)

;;; Call State Management

(defstruct grpc-call
  "Represents a single gRPC call"
  (connection nil :type (or null http2-connection))
  (stream-id nil :type (or null fixnum))
  (service "" :type string)
  (method "" :type string)
  (authority "" :type string)           ; :authority pseudo-header (host:port)
  (timeout nil :type (or null fixnum))  ; Milliseconds
  (metadata nil :type list)             ; List of (name . value) pairs
  (request-sent nil :type boolean)
  (response-headers nil :type (or null list))
  (response-data nil :type (or null (vector (unsigned-byte 8))))
  (response-trailers nil :type (or null list))
  (status nil :type (or null fixnum))
  (status-message nil :type (or null string))
  (error nil :type (or null condition))
  (completed nil :type boolean)
  (lock (bordeaux-threads:make-lock "grpc-call-lock"))
  (condition (bordeaux-threads:make-condition-variable :name "grpc-call-cv")))

;;; Request Sending

(defun call-send-request (call request-bytes)
  "Send gRPC request (HEADERS + DATA frames) over HTTP/2 stream.

   Args:
     call: grpc-call structure
     request-bytes: Serialized protobuf message (raw bytes, not gRPC-framed)

   Returns:
     Stream ID on success, signals error on failure"
  (let* ((conn (grpc-call-connection call))
         (stream-id (http2-connection-next-stream-id conn))
         (hpack-ctx (http2-connection-hpack-encoder conn)))

    ;; Wait for connection to be ready (server SETTINGS received)
    (bt:with-lock-held ((http2-connection-ready-lock conn))
      (loop until (http2-connection-settings-received conn)
            do (bt:condition-wait (http2-connection-ready-cv conn)
                                  (http2-connection-ready-lock conn)
                                  :timeout 5)))  ; 5 second timeout

    ;; Store stream ID in call and increment for next call
    (setf (grpc-call-stream-id call) stream-id)
    (incf (http2-connection-next-stream-id conn) 2)  ; Clients use odd stream IDs

    ;; Register call in connection for response handling BEFORE sending request
    ;; This prevents race condition where response arrives before registration
    (format *error-output* "REGISTER CALL: stream=~D~%" stream-id)
    (setf (gethash stream-id (http2-connection-active-calls conn)) call)

    ;; Build request headers
    (let ((headers (encode-grpc-request-headers
                    (grpc-call-service call)
                    (grpc-call-method call)
                    :authority (grpc-call-authority call)
                    :timeout (grpc-call-timeout call)
                    :metadata (grpc-call-metadata call))))

      ;; Encode headers with HPACK
      (let ((headers-bytes (hpack-encode-headers hpack-ctx headers)))

        ;; Send HEADERS frame
        (let ((headers-frame (make-http2-frame
                              :length (length headers-bytes)
                              :type +frame-type-headers+
                              :flags +flag-end-headers+  ; No CONTINUATION for now
                              :stream-id stream-id
                              :payload headers-bytes)))
          (write-frame-to-stream headers-frame (http2-connection-socket conn)))))

    ;; Frame request with gRPC message framing
    (let ((grpc-message (encode-grpc-message request-bytes)))

      ;; Send DATA frame with END_STREAM
      (let ((data-frame (make-http2-frame
                         :length (length grpc-message)
                         :type +frame-type-data+
                         :flags +flag-end-stream+
                         :stream-id stream-id
                         :payload grpc-message)))
        (write-frame-to-stream data-frame (http2-connection-socket conn))))

    ;; Update call state
    (setf (grpc-call-request-sent call) t)

    stream-id))

;;; Response Receiving

(defun call-receive-response (call &key timeout-ms)
  "Receive gRPC response from HTTP/2 stream.

   Blocks until response is complete or timeout expires.

   Returns:
     (values response-bytes grpc-status grpc-message)

   Signals:
     grpc-error on RPC failure
     timeout-error on timeout"
  (let ((start-time (get-internal-real-time))
        (timeout-internal (when timeout-ms
                           (* timeout-ms (/ internal-time-units-per-second 1000)))))

    (bordeaux-threads:with-lock-held ((grpc-call-lock call))
      ;; Wait for response to complete
      (loop until (grpc-call-completed call)
            do (let ((remaining-time (when timeout-internal
                                      (- timeout-internal
                                         (- (get-internal-real-time) start-time)))))
                 (when (and remaining-time (<= remaining-time 0))
                   (signal-grpc-deadline-exceeded "Call timeout expired"))

                 ;; Wait for condition variable with timeout
                 (bordeaux-threads:condition-wait (grpc-call-condition call)
                                   (grpc-call-lock call)
                                   :timeout (when remaining-time
                                             (/ remaining-time internal-time-units-per-second))))))

    ;; Check for errors
    (when (grpc-call-error call)
      (error (grpc-call-error call)))

    ;; Check gRPC status
    (let ((status (grpc-call-status call)))
      (unless (and status (= status +grpc-status-ok+))
        (signal-grpc-error
         (or status +grpc-status-unknown+)
         (or (grpc-call-status-message call) "Unknown error"))))

    ;; Return response data
    (values (grpc-call-response-data call)
            (grpc-call-status call)
            (grpc-call-status-message call))))

;;; Frame Handling (called by connection's frame dispatcher)

(defun call-handle-headers (call headers end-stream)
  "Handle HEADERS or CONTINUATION frame for this call.

   Args:
     call: grpc-call structure
     headers: Decoded headers (list of (name . value) pairs)
     end-stream: Boolean, true if END_STREAM flag set"
  (format *error-output* "HANDLE HEADERS: end-stream=~A, has-response-headers=~A~%"
          end-stream (not (null (grpc-call-response-headers call))))
  (bordeaux-threads:with-lock-held ((grpc-call-lock call))
    (if (grpc-call-response-headers call)
        ;; This is trailers (second HEADERS frame)
        (progn
          (format *error-output* "  -> Processing as TRAILERS~%")
          (setf (grpc-call-response-trailers call) headers)

          ;; Extract grpc-status and grpc-message from trailers
          (let ((status-header (find "grpc-status" headers :key #'car :test #'string=))
                (message-header (find "grpc-message" headers :key #'car :test #'string=)))
            (when status-header
              (setf (grpc-call-status call)
                    (parse-integer (cdr status-header) :junk-allowed t)))
            (when message-header
              (setf (grpc-call-status-message call) (cdr message-header))))

          (format *error-output* "  -> Marking call COMPLETED, status=~A~%" (grpc-call-status call))
          ;; Call is complete
          (setf (grpc-call-completed call) t)
          (bordeaux-threads:condition-notify (grpc-call-condition call)))

        ;; This is initial response headers
        (progn
          (setf (grpc-call-response-headers call) headers)

          ;; Check for grpc-status in headers (trailers-only response)
          (let ((status-header (find "grpc-status" headers :key #'car :test #'string=)))
            (when status-header
              (setf (grpc-call-status call)
                    (parse-integer (cdr status-header) :junk-allowed t))
              (let ((message-header (find "grpc-message" headers :key #'car :test #'string=)))
                (when message-header
                  (setf (grpc-call-status-message call) (cdr message-header))))

              ;; Trailers-only response - complete now
              (setf (grpc-call-completed call) t)
              (bordeaux-threads:condition-notify (grpc-call-condition call))))

          ;; If END_STREAM but no status, this is an error
          (when (and end-stream (not (grpc-call-status call)))
            (setf (grpc-call-error call)
                  (make-grpc-error :code +grpc-status-unknown+
                                  :message "END_STREAM without grpc-status"))
            (setf (grpc-call-completed call) t)
            (bordeaux-threads:condition-notify (grpc-call-condition call)))))))

(defun call-handle-data (call data end-stream)
  "Handle DATA frame for this call.

   Args:
     call: grpc-call structure
     data: Frame payload bytes
     end-stream: Boolean, true if END_STREAM flag set"
  (bordeaux-threads:with-lock-held ((grpc-call-lock call))
    ;; Accumulate data
    (if (grpc-call-response-data call)
        ;; Append to existing data
        (let* ((existing (grpc-call-response-data call))
               (new-len (+ (length existing) (length data)))
               (result (make-byte-array new-len)))
          (replace result existing)
          (replace result data :start1 (length existing))
          (setf (grpc-call-response-data call) result))

        ;; First data chunk
        (setf (grpc-call-response-data call) (copy-seq data)))

    ;; If END_STREAM without trailers, set default status
    (when (and end-stream (not (grpc-call-response-trailers call)))
      (setf (grpc-call-status call) +grpc-status-ok+)
      (setf (grpc-call-completed call) t)
      (bordeaux-threads:condition-notify (grpc-call-condition call)))))

(defun call-handle-rst-stream (call error-code)
  "Handle RST_STREAM frame for this call.

   Args:
     call: grpc-call structure
     error-code: HTTP/2 error code"
  (bordeaux-threads:with-lock-held ((grpc-call-lock call))
    (setf (grpc-call-error call)
          (make-condition 'grpc-error
                         :code +grpc-status-internal+
                         :message (format nil "Stream reset with error code ~D" error-code)))
    (setf (grpc-call-completed call) t)
    (bordeaux-threads:condition-notify (grpc-call-condition call))))

;;; High-Level Call API

(defun create-grpc-call (connection service method &key authority timeout metadata)
  "Create a new gRPC call.

   Args:
     connection: http2-connection
     service: Service name (e.g., \"helloworld.Greeter\")
     method: Method name (e.g., \"SayHello\")
     authority: Authority/target (e.g., \"localhost:50051\")
     timeout: Timeout in milliseconds (optional)
     metadata: List of (name . value) pairs for custom metadata (optional)

   Returns:
     grpc-call structure"
  (make-grpc-call
   :connection connection
   :service service
   :method method
   :authority authority
   :timeout timeout
   :metadata metadata))

(defun execute-unary-call (call request-bytes)
  "Execute a unary gRPC call (send request, wait for response).

   Args:
     call: grpc-call structure
     request-bytes: Serialized protobuf message (raw bytes)

   Returns:
     Response bytes (gRPC-framed, needs to be decoded)

   Signals:
     grpc-error on RPC failure
     timeout-error on timeout"
  ;; Send request
  (call-send-request call request-bytes)

  ;; Receive response
  (call-receive-response call :timeout-ms (grpc-call-timeout call)))
