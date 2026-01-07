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
             (format stream "gRPC Error ~A: ~A"
                     (grpc-error-status-code condition)
                     (grpc-error-message condition)))))
