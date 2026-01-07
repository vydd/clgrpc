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
