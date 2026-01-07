;;;; errors.lisp - HTTP/2 error codes and conditions (RFC 9113 Section 7)

(in-package #:clgrpc.http2)

;;; HTTP/2 Error Codes (RFC 9113 Section 7)

(defconstant +http2-error-no-error+ #x00
  "The associated condition is not a result of an error.")

(defconstant +http2-error-protocol-error+ #x01
  "The endpoint detected an unspecific protocol error.")

(defconstant +http2-error-internal-error+ #x02
  "The endpoint encountered an unexpected internal error.")

(defconstant +http2-error-flow-control-error+ #x03
  "The endpoint detected that its peer violated the flow-control protocol.")

(defconstant +http2-error-settings-timeout+ #x04
  "The endpoint sent a SETTINGS frame but did not receive a response in a timely manner.")

(defconstant +http2-error-stream-closed+ #x05
  "The endpoint received a frame after a stream was half-closed.")

(defconstant +http2-error-frame-size-error+ #x06
  "The endpoint received a frame with an invalid size.")

(defconstant +http2-error-refused-stream+ #x07
  "The endpoint refused the stream prior to performing any application processing.")

(defconstant +http2-error-cancel+ #x08
  "The endpoint uses this error code to indicate that the stream is no longer needed.")

(defconstant +http2-error-compression-error+ #x09
  "The endpoint is unable to maintain the header compression context for the connection.")

(defconstant +http2-error-connect-error+ #x0a
  "The connection established in response to a CONNECT request was reset or abnormally closed.")

(defconstant +http2-error-enhance-your-calm+ #x0b
  "The endpoint detected that its peer is exhibiting a behavior that might be generating excessive load.")

(defconstant +http2-error-inadequate-security+ #x0c
  "The underlying transport has properties that do not meet minimum security requirements.")

(defconstant +http2-error-http-1-1-required+ #x0d
  "The endpoint requires that HTTP/1.1 be used instead of HTTP/2.")

;;; HTTP/2 Error Conditions

(define-condition http2-error (error)
  ((error-code :initarg :error-code
               :reader http2-error-code
               :type (unsigned-byte 32)
               :documentation "HTTP/2 error code")
   (message :initarg :message
            :reader http2-error-message
            :initform ""
            :type string
            :documentation "Human-readable error message")
   (stream-id :initarg :stream-id
              :reader http2-error-stream-id
              :initform nil
              :type (or null (unsigned-byte 31))
              :documentation "Stream ID if this is a stream error (nil for connection errors)"))
  (:documentation "Base class for HTTP/2 errors")
  (:report (lambda (condition stream)
             (format stream "HTTP/2 Error ~A~@[ on stream ~A~]: ~A"
                     (http2-error-code condition)
                     (http2-error-stream-id condition)
                     (http2-error-message condition)))))

(define-condition http2-protocol-error (http2-error)
  ()
  (:default-initargs :error-code +http2-error-protocol-error+)
  (:documentation "HTTP/2 protocol violation"))

(define-condition http2-flow-control-error (http2-error)
  ()
  (:default-initargs :error-code +http2-error-flow-control-error+)
  (:documentation "HTTP/2 flow control violation"))

(define-condition http2-compression-error (http2-error)
  ()
  (:default-initargs :error-code +http2-error-compression-error+)
  (:documentation "HTTP/2 header compression error"))

(defun signal-http2-error (error-code message &key stream-id)
  "Signal an HTTP/2 error with the given error code and message."
  (error 'http2-error
         :error-code error-code
         :message message
         :stream-id stream-id))

(defun signal-protocol-error (message &key stream-id)
  "Signal an HTTP/2 protocol error."
  (error 'http2-protocol-error
         :message message
         :stream-id stream-id))

(defun signal-flow-control-error (message &key stream-id)
  "Signal an HTTP/2 flow control error."
  (error 'http2-flow-control-error
         :message message
         :stream-id stream-id))

(defun signal-compression-error (message)
  "Signal an HTTP/2 compression error (always connection-level)."
  (error 'http2-compression-error
         :message message))
