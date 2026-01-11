;;;; client.lisp - RouteGuide gRPC client
;;;;
;;;; Demonstrates all four types of gRPC calls.
;;;;
;;;; Usage:
;;;;   (ql:quickload :clgrpc-examples)
;;;;   (clgrpc-examples:routeguide-client-main)

(in-package #:clgrpc-examples)

;;; Test functions for each RPC type

(defun test-get-feature (channel)
  "Test GetFeature (unary RPC)."
  (format t "~%=== Testing GetFeature (Unary RPC) ===~%")
  (let ((point (routeguide:make-point
                :latitude 409146138
                :longitude -746188906)))
    (format t "Looking up feature at lat=~D lon=~D~%"
            (routeguide:point-latitude point)
            (routeguide:point-longitude point))
    (multiple-value-bind (response-bytes status status-message)
        (call-unary channel
                    "routeguide.RouteGuide" "GetFeature"
                    (proto-serialize point))
      (if (= status +grpc-status-ok+)
          (let ((feature (proto-deserialize 'routeguide:feature response-bytes)))
            (if (> (length (routeguide:feature-name feature)) 0)
                (format t "Found feature: ~A~%"
                        (routeguide:feature-name feature))
                (format t "No feature found at that location~%")))
          (format t "Error: ~A (~D)~%" status-message status)))))

(defun test-list-features (channel)
  "Test ListFeatures (server streaming RPC)."
  (format t "~%=== Testing ListFeatures (Server Streaming RPC) ===~%")
  (let ((rect (routeguide:make-rectangle
               :lo (routeguide:make-point
                    :latitude 400000000
                    :longitude -750000000)
               :hi (routeguide:make-point
                    :latitude 420000000
                    :longitude -730000000))))
    (format t "Looking for features in rectangle~%")
    (multiple-value-bind (stream status status-message)
        (call-server-streaming channel
                               "routeguide.RouteGuide" "ListFeatures"
                               (proto-serialize rect))
      (if (null stream)
          (format t "Error: ~A (~D)~%" status-message status)
          (let ((count 0))
            (loop for msg-bytes = (stream-recv stream)
                  while msg-bytes
                  do (let ((feature (proto-deserialize 'routeguide:feature msg-bytes)))
                       (incf count)
                       (format t "  ~D. ~A~%"
                               count
                               (routeguide:feature-name feature))))
            (format t "Received ~D features~%" count))))))

(defun test-record-route (channel)
  "Test RecordRoute (client streaming RPC)."
  (format t "~%=== Testing RecordRoute (Client Streaming RPC) ===~%")
  (let ((points (list
                 (routeguide:make-point :latitude 406337092 :longitude -740122226)
                 (routeguide:make-point :latitude 406421967 :longitude -747727624)
                 (routeguide:make-point :latitude 404318328 :longitude -740835638)
                 (routeguide:make-point :latitude 405957808 :longitude -743255336)
                 (routeguide:make-point :latitude 406411633 :longitude -741722051))))
    (format t "Sending ~D points~%" (length points))
    (multiple-value-bind (stream status status-message)
        (call-client-streaming channel
                               "routeguide.RouteGuide" "RecordRoute")
      (if (null stream)
          (format t "Error: ~A (~D)~%" status-message status)
          (progn
            (dolist (point points)
              (format t "  Sending point lat=~D lon=~D~%"
                      (routeguide:point-latitude point)
                      (routeguide:point-longitude point))
              (stream-send stream (proto-serialize point)))
            ;; Close send side and receive response
            (stream-close-send stream)
            (let ((response-bytes (stream-recv stream)))
              (if response-bytes
                  (let ((summary (proto-deserialize 'routeguide:route-summary response-bytes)))
                    (format t "~%Route summary:~%")
                    (format t "  Points: ~D~%"
                            (routeguide:route-summary-point-count summary))
                    (format t "  Features: ~D~%"
                            (routeguide:route-summary-feature-count summary))
                    (format t "  Distance: ~D meters~%"
                            (routeguide:route-summary-distance summary))
                    (format t "  Time: ~D seconds~%"
                            (routeguide:route-summary-elapsed-time summary)))
                  (format t "Error: No response received~%"))))))))

(defun test-route-chat (channel)
  "Test RouteChat (bidirectional streaming RPC)."
  (format t "~%=== Testing RouteChat (Bidirectional Streaming RPC) ===~%")
  (let ((notes (list
                (routeguide:make-route-note
                 :location (routeguide:make-point
                            :latitude 409146138
                            :longitude -746188906)
                 :message "First message at Berkshire Valley")
                (routeguide:make-route-note
                 :location (routeguide:make-point
                            :latitude 411633217
                            :longitude -746784394)
                 :message "Second message at Jersey Avenue")
                (routeguide:make-route-note
                 :location (routeguide:make-point
                            :latitude 409146138
                            :longitude -746188906)
                 :message "Third message at Berkshire Valley")
                (routeguide:make-route-note
                 :location (routeguide:make-point
                            :latitude 411633217
                            :longitude -746784394)
                 :message "Fourth message at Jersey Avenue"))))
    (format t "Starting chat with ~D notes~%" (length notes))
    (multiple-value-bind (stream status status-message)
        (call-bidirectional-streaming channel
                                      "routeguide.RouteGuide" "RouteChat")
      (if (null stream)
          (format t "Error: ~A (~D)~%" status-message status)
          (progn
            (dolist (note notes)
              (format t "~%Sending note: ~A~%"
                      (routeguide:route-note-message note))
              (stream-send stream (proto-serialize note))
              (sleep 0.1)
              (loop for msg-bytes = (stream-recv stream :timeout-ms 100)
                    while msg-bytes
                    do (let ((received-note (proto-deserialize 'routeguide:route-note msg-bytes)))
                         (format t "  Got note: ~A~%"
                                 (routeguide:route-note-message received-note)))))
            (stream-close-send stream)
            (format t "~%Chat completed successfully~%"))))))

;;; Main

(defun routeguide-client-main ()
  "Run RouteGuide client tests."
  (format t "~%RouteGuide gRPC Client~%")
  (format t "======================~%")
  (let ((channel (make-channel "localhost:50051" :secure nil)))
    (handler-case
        (progn
          (test-get-feature channel)
          (test-list-features channel)
          (test-record-route channel)
          (test-route-chat channel)
          (format t "~%~%All tests completed!~%"))
      (error (e)
        (format t "~%Error: ~A~%" e)))
    (close-channel channel)
    (format t "Channel closed~%")))
