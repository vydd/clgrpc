;;;; metadata.lisp - gRPC metadata and header encoding
;;;
;;; gRPC uses HTTP/2 headers for metadata.
;;; Request headers include pseudo-headers and custom metadata.
;;; Response headers are minimal.
;;; Trailers contain grpc-status and grpc-message.

(in-package #:clgrpc.grpc)

;;; gRPC Header Constants

(defconstant +grpc-content-type+ "application/grpc+proto"
  "gRPC content type for protobuf messages")

(defconstant +grpc-te+ "trailers"
  "TE header value required by gRPC")

;;; Metadata Encoding
;;;
;;; gRPC metadata keys:
;;;   - ASCII keys: lowercase letters, digits, _, -, .
;;;   - Binary keys: end with "-bin", value is base64-encoded
;;;
;;; Header format:
;;;   ASCII:  (name . value) where both are strings
;;;   Binary: (name-bin . base64-string)

(defun metadata-key-binary-p (key)
  "Check if metadata key represents binary data (ends with -bin)"
  (let ((key-str (if (keywordp key) (string-downcase (symbol-name key)) key)))
    (and (>= (length key-str) 4)
         (string= key-str "-bin" :start1 (- (length key-str) 4)))))

(defun encode-metadata-value (value binary-p)
  "Encode metadata value (base64 if binary, string if ASCII)"
  (if binary-p
      ;; Binary value - base64 encode
      (if (stringp value)
          (base64-encode-string value)
          (base64-encode-bytes value))
      ;; ASCII value - use as-is
      (if (stringp value)
          value
          (princ-to-string value))))

(defun decode-metadata-value (value binary-p)
  "Decode metadata value (base64 if binary, string if ASCII)"
  (if binary-p
      ;; Binary value - base64 decode
      (base64-decode-string value)
      ;; ASCII value - use as-is
      value))

(defun base64-encode-string (string)
  "Encode string to base64 (stub - to be implemented)"
  ;; TODO: Implement proper base64 encoding
  ;; For now, just use the string as-is
  string)

(defun base64-encode-bytes (bytes)
  "Encode bytes to base64 string (stub - to be implemented)"
  ;; TODO: Implement proper base64 encoding
  ;; For now, convert to string
  (babel:octets-to-string bytes :encoding :latin-1))

(defun base64-decode-string (string)
  "Decode base64 string to bytes (stub - to be implemented)"
  ;; TODO: Implement proper base64 decoding
  ;; For now, convert from string
  (babel:string-to-octets string :encoding :latin-1))

(defun normalize-metadata-key (key)
  "Normalize metadata key to lowercase string"
  (cond
    ((keywordp key)
     (string-downcase (symbol-name key)))
    ((stringp key)
     (string-downcase key))
    (t
     (string-downcase (princ-to-string key)))))

(defun encode-metadata (metadata)
  "Encode gRPC metadata to HTTP/2 headers.

   Args:
     metadata: Alist of (key . value) pairs

   Returns:
     List of (name . value) header pairs for HTTP/2"
  (loop for (key . value) in metadata
        for key-str = (normalize-metadata-key key)
        for binary-p = (metadata-key-binary-p key-str)
        collect (cons key-str (encode-metadata-value value binary-p))))

(defun decode-metadata (headers)
  "Decode HTTP/2 headers to gRPC metadata.

   Args:
     headers: List of (name . value) header pairs

   Returns:
     Alist of decoded metadata"
  (loop for (key . value) in headers
        unless (or (char= (char key 0) #\:)  ; Skip pseudo-headers
                   (string= key "content-type")
                   (string= key "te")
                   (string= key "grpc-status")
                   (string= key "grpc-message"))
        collect (cons key (decode-metadata-value value (metadata-key-binary-p key)))))

;;; Timeout Encoding
;;;
;;; gRPC timeout format: <value><unit>
;;;   H = hours, M = minutes, S = seconds, m = milliseconds, u = microseconds, n = nanoseconds

(defun encode-grpc-timeout (timeout-ms)
  "Encode timeout in milliseconds to gRPC timeout string.

   Args:
     timeout-ms: Timeout in milliseconds

   Returns:
     String like \"1000m\" or \"10S\""
  (cond
    ((>= timeout-ms 3600000)
     (format nil "~DH" (floor timeout-ms 3600000)))
    ((>= timeout-ms 60000)
     (format nil "~DM" (floor timeout-ms 60000)))
    ((>= timeout-ms 1000)
     (format nil "~DS" (floor timeout-ms 1000)))
    (t
     (format nil "~Dm" timeout-ms))))

(defun decode-grpc-timeout (timeout-str)
  "Decode gRPC timeout string to milliseconds.

   Args:
     timeout-str: String like \"1000m\" or \"10S\"

   Returns:
     Timeout in milliseconds"
  (let* ((len (length timeout-str))
         (unit (char timeout-str (1- len)))
         (value (parse-integer timeout-str :end (1- len))))
    (case unit
      (#\H (* value 3600000))   ; Hours
      (#\M (* value 60000))     ; Minutes
      (#\S (* value 1000))      ; Seconds
      (#\m value)               ; Milliseconds
      (#\u (floor value 1000))  ; Microseconds
      (#\n (floor value 1000000))  ; Nanoseconds
      (t (signal-grpc-invalid-argument
          (format nil "Invalid timeout unit: ~C" unit))))))

;;; Request Headers

(defun encode-grpc-request-headers (service method &key authority timeout metadata)
  "Encode gRPC request headers for HTTP/2 HEADERS frame.

   Args:
     service: Service name (e.g., \"helloworld.Greeter\")
     method: Method name (e.g., \"SayHello\")
     authority: Authority/host (e.g., \"localhost:50051\")
     timeout: Timeout in milliseconds (optional)
     metadata: Custom metadata alist (optional)

   Returns:
     List of (name . value) header pairs"
  (let ((headers (list
                  (cons ":method" "POST")
                  (cons ":scheme" "http")  ; Will be "https" for TLS
                  (cons ":path" (format nil "/~A/~A" service method))
                  (cons ":authority" (or authority "localhost"))
                  (cons "content-type" +grpc-content-type+)
                  (cons "te" +grpc-te+))))

    ;; Add timeout if specified
    (when timeout
      (push (cons "grpc-timeout" (encode-grpc-timeout timeout)) headers))

    ;; Add custom metadata
    (when metadata
      (setf headers (append headers (encode-metadata metadata))))

    headers))

;;; Response Headers

(defun encode-grpc-response-headers (&key metadata)
  "Encode gRPC response headers for HTTP/2 HEADERS frame.

   Args:
     metadata: Custom metadata alist (optional)

   Returns:
     List of (name . value) header pairs"
  (let ((headers (list
                  (cons ":status" "200")
                  (cons "content-type" +grpc-content-type+))))

    ;; Add custom metadata
    (when metadata
      (setf headers (append headers (encode-metadata metadata))))

    headers))

;;; Trailers (grpc-status and grpc-message)

(defun encode-grpc-trailers (status-code &optional message)
  "Encode gRPC trailers with status and optional message.

   Args:
     status-code: gRPC status code (0-16)
     message: Optional status message

   Returns:
     List of (name . value) header pairs for trailers"
  (let ((trailers (list
                   (cons "grpc-status" (princ-to-string status-code)))))

    (when (and message (not (string= message "")))
      (push (cons "grpc-message" message) trailers))

    trailers))

(defun decode-grpc-trailers (headers)
  "Decode gRPC trailers to extract status and message.

   Args:
     headers: List of (name . value) header pairs

   Returns:
     (values status-code message)"
  (let ((status-code nil)
        (message ""))

    (dolist (header headers)
      (let ((name (car header))
            (value (cdr header)))
        (cond
          ((string= name "grpc-status")
           (setf status-code (parse-integer value)))
          ((string= name "grpc-message")
           (setf message value)))))

    (values (or status-code +grpc-status-unknown+) message)))

;;; Utilities

(defun grpc-content-type-p (content-type)
  "Check if content-type is a valid gRPC content type"
  (and (stringp content-type)
       (or (string= content-type "application/grpc")
           (alexandria:starts-with-subseq "application/grpc+" content-type))))
