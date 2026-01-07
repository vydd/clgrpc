;;;; status.lisp - gRPC status codes (RFC-like specification)

(in-package #:clgrpc.grpc)

;;; gRPC Status Codes

(defconstant +grpc-status-ok+ 0
  "Not an error; returned on success.")

(defconstant +grpc-status-cancelled+ 1
  "The operation was cancelled, typically by the caller.")

(defconstant +grpc-status-unknown+ 2
  "Unknown error.")

(defconstant +grpc-status-invalid-argument+ 3
  "The client specified an invalid argument.")

(defconstant +grpc-status-deadline-exceeded+ 4
  "The deadline expired before the operation could complete.")

(defconstant +grpc-status-not-found+ 5
  "Some requested entity was not found.")

(defconstant +grpc-status-already-exists+ 6
  "The entity that a client attempted to create already exists.")

(defconstant +grpc-status-permission-denied+ 7
  "The caller does not have permission to execute the specified operation.")

(defconstant +grpc-status-resource-exhausted+ 8
  "Some resource has been exhausted.")

(defconstant +grpc-status-failed-precondition+ 9
  "The operation was rejected because the system is not in a state required for the operation's execution.")

(defconstant +grpc-status-aborted+ 10
  "The operation was aborted.")

(defconstant +grpc-status-out-of-range+ 11
  "The operation was attempted past the valid range.")

(defconstant +grpc-status-unimplemented+ 12
  "The operation is not implemented or is not supported/enabled in this service.")

(defconstant +grpc-status-internal+ 13
  "Internal errors.")

(defconstant +grpc-status-unavailable+ 14
  "The service is currently unavailable.")

(defconstant +grpc-status-data-loss+ 15
  "Unrecoverable data loss or corruption.")

(defconstant +grpc-status-unauthenticated+ 16
  "The request does not have valid authentication credentials for the operation.")

;;; Status Code Utilities

(defun valid-status-code-p (code)
  "Check if code is a valid gRPC status code (0-16)"
  (and (integerp code) (<= 0 code 16)))

(defun status-code-name (code)
  "Get human-readable name for status code"
  (case code
    (0 "OK")
    (1 "CANCELLED")
    (2 "UNKNOWN")
    (3 "INVALID_ARGUMENT")
    (4 "DEADLINE_EXCEEDED")
    (5 "NOT_FOUND")
    (6 "ALREADY_EXISTS")
    (7 "PERMISSION_DENIED")
    (8 "RESOURCE_EXHAUSTED")
    (9 "FAILED_PRECONDITION")
    (10 "ABORTED")
    (11 "OUT_OF_RANGE")
    (12 "UNIMPLEMENTED")
    (13 "INTERNAL")
    (14 "UNAVAILABLE")
    (15 "DATA_LOSS")
    (16 "UNAUTHENTICATED")
    (t "UNKNOWN")))

(defun retryable-status-p (code)
  "Check if status code indicates a retryable error"
  (member code '(1   ; CANCELLED
                 4   ; DEADLINE_EXCEEDED
                 8   ; RESOURCE_EXHAUSTED
                 10  ; ABORTED
                 14  ; UNAVAILABLE
                 )))
