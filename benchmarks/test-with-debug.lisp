;;;; test-with-debug.lisp - Test with debug output and embedded server
;;;;
;;;; Prerequisites: (ql:quickload :clgrpc-examples)
;;;; Usage: sbcl --load test-with-debug.lisp

(ql:quickload :clgrpc-examples :silent t)

;; Enable debug
(setf clgrpc.http2:*debug* t)

(in-package #:clgrpc-examples)

;; Load features for the server
(defparameter *test-route-features*
  (routeguide-load-features
   (asdf:system-relative-pathname :clgrpc-examples "routeguide/route_guide_db.json")))

;; Start server
(let ((server (clgrpc.server:make-server :port 50054)))
  (let ((handler (make-instance 'route-guide-handler)))
    ;; Set the features
    (setf *route-features* *test-route-features*)

    (clgrpc.server:register-handler (clgrpc.server:grpc-server-router server)
                                    "routeguide.RouteGuide" "GetFeature"
                                    handler :rpc-type :unary))
  (clgrpc.server:start-server server)
  (sleep 1)

  ;; Test one call
  (let* ((channel (clgrpc.client:make-channel "localhost:50054" :secure nil))
         (point (routeguide:make-point :latitude 409146138 :longitude -746188906))
         (request-bytes (clgrpc.grpc:proto-serialize point)))

    (format t "~%=== Testing one call ===~%")
    (multiple-value-bind (response-bytes status status-message)
        (clgrpc.client:call-unary channel "routeguide.RouteGuide" "GetFeature"
                                  request-bytes :timeout 5000)
      (declare (ignore status-message))
      (format t "~%Status: ~A~%" status)
      (format t "Got response: ~D bytes~%" (length response-bytes))
      (clgrpc.client:close-channel channel)))

  (clgrpc.server:stop-server server)
  (format t "~%Test complete!~%"))

(quit)
