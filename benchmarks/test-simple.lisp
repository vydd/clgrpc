;;;; test-simple.lisp - Simple gRPC call test
;;;;
;;;; Prerequisites: (ql:quickload :clgrpc-examples)
;;;; Note: Requires RouteGuide server on port 50054
;;;; Usage: sbcl --load test-simple.lisp

(ql:quickload :clgrpc-examples :silent t)

(in-package #:clgrpc.client)

(format t "~%Making channel...~%")
(let ((channel (make-channel "localhost:50054" :secure nil))
      (point (routeguide:make-point :latitude 409146138 :longitude -746188906)))
  (let ((request-bytes (clgrpc.grpc:proto-serialize point)))
    (format t "Calling GetFeature...~%")
    (multiple-value-bind (response-bytes status status-message)
        (call-unary channel "routeguide.RouteGuide" "GetFeature"
                   request-bytes :timeout 5000)
      (declare (ignore status-message))
      (format t "Status: ~A~%" status)
      (format t "Response bytes: ~A~%" (length response-bytes))
      (close-channel channel)
      (format t "Success!~%"))))

(quit)
