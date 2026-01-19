;;;; interceptors.lisp - Server interceptor middleware
;;;
;;; Interceptors allow wrapping handlers for cross-cutting concerns:
;;; - Logging
;;; - Authentication/authorization
;;; - Metrics collection
;;; - Request validation
;;; - Error handling
;;;
;;; Interceptors can inspect and modify:
;;; - Request metadata (headers)
;;; - Request/response messages
;;; - Handler context
;;; - Status codes and messages
;;;
;;; They can also short-circuit execution (e.g., for auth failures).

(in-package #:clgrpc.server)

;;; Interceptor Protocol
;;;
;;; Unary interceptor signature:
;;;   (lambda (request-bytes context info continuation)
;;;     ... pre-processing ...
;;;     (multiple-value-bind (response-bytes status-code status-message metadata)
;;;         (funcall continuation request-bytes context)
;;;       ... post-processing ...
;;;       (values response-bytes status-code status-message metadata)))
;;;
;;; Stream interceptor signature:
;;;   (lambda (stream context info continuation)
;;;     ... wrapping logic ...
;;;     (funcall continuation stream context)
;;;     ... cleanup ...)
;;;
;;; Where:
;;;   request-bytes: Raw protobuf message bytes
;;;   stream: grpc-server-stream for streaming RPCs
;;;   context: handler-context with metadata, deadline, etc.
;;;   info: interceptor-info with service/method names
;;;   continuation: Function to call next interceptor or handler

(defstruct interceptor-info
  "Information about the RPC being intercepted"
  (service nil :type (or null string))
  (method nil :type (or null string))
  (rpc-type nil :type (or null keyword)))  ; :unary, :client-streaming, etc.

;;; Chain Execution

(defun execute-unary-interceptor-chain (interceptors handler request-bytes context info)
  "Execute chain of unary interceptors, ending with handler.

   Args:
     interceptors: List of interceptor functions
     handler: Final handler to invoke
     request-bytes: Request message bytes
     context: handler-context
     info: interceptor-info

   Returns:
     (values response-bytes status-code status-message metadata)"
  (labels ((make-continuation (remaining-interceptors)
             (if remaining-interceptors
                 ;; More interceptors to execute
                 (lambda (req ctx)
                   (funcall (car remaining-interceptors)
                            req ctx info
                            (make-continuation (cdr remaining-interceptors))))
                 ;; No more interceptors - call handler
                 (lambda (req ctx)
                   (funcall handler req ctx)))))
    (let ((continuation (make-continuation interceptors)))
      (funcall continuation request-bytes context))))

(defun execute-stream-interceptor-chain (interceptors handler stream context info)
  "Execute chain of stream interceptors, ending with handler.

   Args:
     interceptors: List of interceptor functions
     handler: Final handler to invoke
     stream: grpc-server-stream
     context: handler-context
     info: interceptor-info

   Returns:
     (values status-code status-message metadata)"
  (labels ((make-continuation (remaining-interceptors)
             (if remaining-interceptors
                 ;; More interceptors to execute
                 (lambda (s ctx)
                   (funcall (car remaining-interceptors)
                            s ctx info
                            (make-continuation (cdr remaining-interceptors))))
                 ;; No more interceptors - call handler
                 (lambda (s ctx)
                   (funcall handler s ctx)))))
    (let ((continuation (make-continuation interceptors)))
      (funcall continuation stream context))))

;;; Built-in Interceptors

(defun logging-interceptor (request-bytes context info continuation)
  "Logs request/response for each RPC.

   Example usage:
     (add-unary-interceptor server #'logging-interceptor)"
  (let ((start-time (get-internal-real-time))
        (service (interceptor-info-service info))
        (method (interceptor-info-method info)))

    (format t "[~A] --> ~A/~A (req-size: ~D bytes)~%"
            (format-timestamp) service method (length request-bytes))

    (multiple-value-bind (response-bytes status-code status-message metadata)
        (funcall continuation request-bytes context)

      (let* ((end-time (get-internal-real-time))
             (duration-ms (/ (- end-time start-time)
                            (/ internal-time-units-per-second 1000))))
        (format t "[~A] <-- ~A/~A status=~D duration=~,2fms (resp-size: ~D bytes)~%"
                (format-timestamp) service method status-code duration-ms
                (if response-bytes (length response-bytes) 0)))

      (values response-bytes status-code status-message metadata))))

(defun stream-logging-interceptor (stream context info continuation)
  "Logs stream lifecycle for streaming RPCs.

   Example usage:
     (add-stream-interceptor server #'stream-logging-interceptor)"
  (let ((start-time (get-internal-real-time))
        (service (interceptor-info-service info))
        (method (interceptor-info-method info))
        (rpc-type (interceptor-info-rpc-type info)))

    (format t "[~A] --> ~A/~A (~A streaming started)~%"
            (format-timestamp) service method rpc-type)

    (unwind-protect
        (funcall continuation stream context)
      (let* ((end-time (get-internal-real-time))
             (duration-ms (/ (- end-time start-time)
                            (/ internal-time-units-per-second 1000))))
        (format t "[~A] <-- ~A/~A (~A streaming ended, duration=~,2fms)~%"
                (format-timestamp) service method rpc-type duration-ms)))))

(defun metadata-validator-interceptor (required-keys)
  "Creates an interceptor that validates required metadata keys.

   Args:
     required-keys: List of required metadata key strings (e.g., '(\"authorization\" \"x-request-id\"))

   Returns:
     Interceptor function

   Example usage:
     (add-unary-interceptor server
       (metadata-validator-interceptor '(\"authorization\")))"
  (lambda (request-bytes context info continuation)
    (block validate
      (let ((metadata (handler-context-metadata context)))
        ;; Check for required keys
        (dolist (key required-keys)
          (unless (assoc key metadata :test #'string-equal)
            ;; Missing required key - return error
            (return-from validate
              (values nil
                      +grpc-status-unauthenticated+
                      (format nil "Missing required metadata: ~A" key)
                      nil))))
        ;; All required keys present - continue
        (funcall continuation request-bytes context)))))

(defun auth-interceptor (auth-fn)
  "Creates an interceptor that authenticates requests using auth-fn.

   Args:
     auth-fn: (lambda (metadata) ...) that returns T if authenticated, NIL otherwise

   Returns:
     Interceptor function

   Example usage:
     (add-unary-interceptor server
       (auth-interceptor
         (lambda (metadata)
           (let ((auth-header (cdr (assoc \"authorization\" metadata :test #'string-equal))))
             (and auth-header (string= auth-header \"Bearer secret-token\"))))))"
  (lambda (request-bytes context info continuation)
    (let ((metadata (handler-context-metadata context)))
      (if (funcall auth-fn metadata)
          ;; Authenticated - continue
          (funcall continuation request-bytes context)
          ;; Not authenticated - return error
          (values nil
                  +grpc-status-unauthenticated+
                  "Authentication failed"
                  nil)))))

(defun timeout-interceptor (timeout-seconds)
  "Creates an interceptor that enforces a maximum handler execution time.

   Args:
     timeout-seconds: Maximum seconds to allow handler to run

   Returns:
     Interceptor function

   Example usage:
     (add-unary-interceptor server (timeout-interceptor 5))  ; 5 second timeout"
  (lambda (request-bytes context info continuation)
    (declare (ignore info))
    (let ((result nil)
          (completed nil)
          (lock (bt:make-lock))
          (cv (bt:make-condition-variable)))

      ;; Run handler in separate thread
      (bt:make-thread
       (lambda ()
         (let ((res (multiple-value-list (funcall continuation request-bytes context))))
           (bt:with-lock-held (lock)
             (setf result res
                   completed t)
             (bt:condition-notify cv)))))

      ;; Wait for completion or timeout
      (bt:with-lock-held (lock)
        (bt:condition-wait cv lock :timeout timeout-seconds)
        (if completed
            ;; Completed in time
            (values-list result)
            ;; Timeout
            (values nil
                    +grpc-status-deadline-exceeded+
                    (format nil "Handler exceeded timeout of ~D seconds" timeout-seconds)
                    nil))))))

;;; Utility

(defun format-timestamp ()
  "Format current time as HH:MM:SS"
  (multiple-value-bind (sec min hour)
      (decode-universal-time (get-universal-time))
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))
