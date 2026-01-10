;;;; client.lisp - RouteGuide gRPC client
;;;;
;;;; Demonstrates all four types of gRPC calls

;; Load Quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Install dependencies
(ql:quickload '(:cl+ssl :usocket :bordeaux-threads
                :alexandria :trivial-gray-streams :fast-io :babel)
              :silent t)

;; Add current directory to ASDF registry
(push (make-pathname :name nil :type nil
                     :defaults (merge-pathnames "../../" *load-truename*))
      asdf:*central-registry*)

;; Load the system
(format t "~%Loading clgrpc...~%")
(asdf:load-system :clgrpc)

;; Load RouteGuide protobuf definitions
(load (merge-pathnames "routeguide-proto.lisp" *load-truename*))

(in-package #:clgrpc.client)

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

      (if (= status clgrpc.grpc:+grpc-status-ok+)
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
          (progn
            ;; Receive all features
            (let ((count 0))
              (loop for msg-bytes = (client-stream-recv stream)
                    while msg-bytes
                    do (let ((feature (proto-deserialize 'routeguide:feature msg-bytes)))
                         (incf count)
                         (format t "  ~D. ~A~%"
                                 count
                                 (routeguide:feature-name feature))))
              (format t "Received ~D features~%" count)))))))

(defun test-record-route (channel)
  "Test RecordRoute (client streaming RPC)."
  (format t "~%=== Testing RecordRoute (Client Streaming RPC) ===~%")

  ;; Create a route with several points
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
            ;; Send all points
            (dolist (point points)
              (format t "  Sending point lat=~D lon=~D~%"
                      (routeguide:point-latitude point)
                      (routeguide:point-longitude point))
              (client-stream-send stream
                                 (proto-serialize point)))

            ;; Close and get response
            (multiple-value-bind (response-bytes final-status final-message)
                (client-stream-close-and-recv stream)

              (if (= final-status clgrpc.grpc:+grpc-status-ok+)
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
                  (format t "Error: ~A (~D)~%" final-message final-status))))))))

(defun test-route-chat (channel)
  "Test RouteChat (bidirectional streaming RPC)."
  (format t "~%=== Testing RouteChat (Bidirectional Streaming RPC) ===~%")

  ;; Create some notes at different locations
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
            ;; Send and receive notes
            (dolist (note notes)
              (format t "~%Sending note: ~A~%"
                      (routeguide:route-note-message note))
              (client-stream-send stream
                                 (proto-serialize note))

              ;; Wait a bit for responses
              (sleep 0.1)

              ;; Receive any responses
              (loop for msg-bytes = (client-stream-recv stream :timeout-ms 100)
                    while msg-bytes
                    do (let ((received-note (proto-deserialize 'routeguide:route-note msg-bytes)))
                         (format t "  Got note: ~A~%"
                                 (routeguide:route-note-message received-note)))))

            ;; Close stream
            (multiple-value-bind (final-status final-message)
                (client-stream-close stream)
              (if (= final-status clgrpc.grpc:+grpc-status-ok+)
                  (format t "~%Chat completed successfully~%")
                  (format t "~%Error: ~A (~D)~%" final-message final-status))))))))

;;; Main

(defun main ()
  "Run RouteGuide client tests."
  (format t "~%RouteGuide gRPC Client~%")
  (format t "======================~%")

  (let ((channel (make-channel "localhost:50051" :secure nil)))

    (handler-case
        (progn
          ;; Test 1: Unary RPC
          (test-get-feature channel)

          ;; Test 2: Server streaming RPC
          (test-list-features channel)

          ;; Test 3: Client streaming RPC
          (test-record-route channel)

          ;; Test 4: Bidirectional streaming RPC
          (test-route-chat channel)

          (format t "~%~%All tests completed!~%"))

      (error (e)
        (format t "~%Error: ~A~%" e)))

    (close-channel channel)
    (format t "Channel closed~%")))

;; Run if called with --run
(when (member "--run" sb-ext:*posix-argv* :test #'string=)
  (main))
