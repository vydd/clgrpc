;;;; errors.lisp - gRPC error conditions

(in-package #:clgrpc.grpc)

(define-condition grpc-error (error)
  ((status-code :initarg :status-code
                :reader grpc-error-status-code
                :type (unsigned-byte 32))
   (message :initarg :message
            :reader grpc-error-message
            :initform ""
            :type string)
   (details :initarg :details
            :reader grpc-error-details
            :initform nil))
  (:documentation "gRPC error condition")
  (:report (lambda (condition stream)
             (format stream "gRPC Error ~A (~A): ~A"
                     (status-code-name (grpc-error-status-code condition))
                     (grpc-error-status-code condition)
                     (grpc-error-message condition)))))

;;; Specific Error Types

(define-condition grpc-cancelled (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-cancelled+)
  (:documentation "Operation was cancelled"))

(define-condition grpc-invalid-argument (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-invalid-argument+)
  (:documentation "Invalid argument provided"))

(define-condition grpc-deadline-exceeded (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-deadline-exceeded+)
  (:documentation "Deadline exceeded"))

(define-condition grpc-not-found (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-not-found+)
  (:documentation "Entity not found"))

(define-condition grpc-already-exists (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-already-exists+)
  (:documentation "Entity already exists"))

(define-condition grpc-permission-denied (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-permission-denied+)
  (:documentation "Permission denied"))

(define-condition grpc-resource-exhausted (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-resource-exhausted+)
  (:documentation "Resource exhausted"))

(define-condition grpc-failed-precondition (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-failed-precondition+)
  (:documentation "Precondition failed"))

(define-condition grpc-aborted (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-aborted+)
  (:documentation "Operation aborted"))

(define-condition grpc-out-of-range (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-out-of-range+)
  (:documentation "Out of range"))

(define-condition grpc-unimplemented (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-unimplemented+)
  (:documentation "Operation not implemented"))

(define-condition grpc-internal (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-internal+)
  (:documentation "Internal error"))

(define-condition grpc-unavailable (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-unavailable+)
  (:documentation "Service unavailable"))

(define-condition grpc-data-loss (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-data-loss+)
  (:documentation "Data loss"))

(define-condition grpc-unauthenticated (grpc-error)
  ()
  (:default-initargs :status-code +grpc-status-unauthenticated+)
  (:documentation "Request not authenticated"))

;;; Error Construction and Signaling

(defun make-grpc-error (status-code message &optional details)
  "Create a grpc-error condition with given status code and message"
  (make-condition 'grpc-error
                  :status-code status-code
                  :message message
                  :details details))

(defun signal-grpc-error (status-code message &optional details)
  "Signal a gRPC error with given status code and message"
  (error 'grpc-error
         :status-code status-code
         :message message
         :details details))

(defun signal-grpc-cancelled (message &optional details)
  "Signal CANCELLED error"
  (error 'grpc-cancelled :message message :details details))

(defun signal-grpc-invalid-argument (message &optional details)
  "Signal INVALID_ARGUMENT error"
  (error 'grpc-invalid-argument :message message :details details))

(defun signal-grpc-deadline-exceeded (message &optional details)
  "Signal DEADLINE_EXCEEDED error"
  (error 'grpc-deadline-exceeded :message message :details details))

(defun signal-grpc-not-found (message &optional details)
  "Signal NOT_FOUND error"
  (error 'grpc-not-found :message message :details details))

(defun signal-grpc-already-exists (message &optional details)
  "Signal ALREADY_EXISTS error"
  (error 'grpc-already-exists :message message :details details))

(defun signal-grpc-permission-denied (message &optional details)
  "Signal PERMISSION_DENIED error"
  (error 'grpc-permission-denied :message message :details details))

(defun signal-grpc-resource-exhausted (message &optional details)
  "Signal RESOURCE_EXHAUSTED error"
  (error 'grpc-resource-exhausted :message message :details details))

(defun signal-grpc-failed-precondition (message &optional details)
  "Signal FAILED_PRECONDITION error"
  (error 'grpc-failed-precondition :message message :details details))

(defun signal-grpc-aborted (message &optional details)
  "Signal ABORTED error"
  (error 'grpc-aborted :message message :details details))

(defun signal-grpc-out-of-range (message &optional details)
  "Signal OUT_OF_RANGE error"
  (error 'grpc-out-of-range :message message :details details))

(defun signal-grpc-unimplemented (message &optional details)
  "Signal UNIMPLEMENTED error"
  (error 'grpc-unimplemented :message message :details details))

(defun signal-grpc-internal (message &optional details)
  "Signal INTERNAL error"
  (error 'grpc-internal :message message :details details))

(defun signal-grpc-unavailable (message &optional details)
  "Signal UNAVAILABLE error"
  (error 'grpc-unavailable :message message :details details))

(defun signal-grpc-data-loss (message &optional details)
  "Signal DATA_LOSS error"
  (error 'grpc-data-loss :message message :details details))

(defun signal-grpc-unauthenticated (message &optional details)
  "Signal UNAUTHENTICATED error"
  (error 'grpc-unauthenticated :message message :details details))
