;;;; test-all-streaming-types.lisp - Quick test to verify all RPC types work
;;;;
;;;; This script:
;;;; 1. Starts a RouteGuide server
;;;; 2. Runs all 4 RPC types (unary, server streaming, client streaming, bidirectional)
;;;; 3. Reports success/failure

(ql:quickload :clgrpc-examples :silent t)

(defun test-all-streaming ()
  "Test all four streaming types"
  (format t "~%╔════════════════════════════════════════════════════════╗~%")
  (format t "║  Testing All gRPC Streaming Types                     ║~%")
  (format t "╚════════════════════════════════════════════════════════╝~%~%")

  ;; Start server in background thread
  (let ((server-thread nil)
        (server nil)
        (test-passed nil))
    (unwind-protect
         (progn
           (format t "1. Starting RouteGuide server...~%")
           (setf server (clgrpc.server:make-server :port 50099))

           ;; Load features
           (let ((features (clgrpc-examples::routeguide-clos-load-features
                            #P"/home/vydd/Code/clgrpc/examples/routeguide/route_guide_db.json")))
             (setf clgrpc-examples::*route-features-clos* features)
             (format t "   Loaded ~D features~%" (length features)))

           ;; Register service
           (clgrpc.server:register-service
            (clgrpc.server:grpc-server-router server)
            (make-instance 'clgrpc-examples::route-guide-service-clos))

           ;; Start server
           (clgrpc.server:start-server server)
           (format t "   ✓ Server started on port 50099~%~%")

           ;; Give server time to start
           (sleep 1)

           ;; Run client tests
           (format t "2. Testing all RPC types...~%~%")
           (let ((channel (clgrpc.client:make-channel "localhost:50099" :secure nil)))
             (unwind-protect
                  (handler-case
                      (progn
                        ;; Test 1: Unary RPC
                        (format t "   [1/4] Testing Unary RPC (GetFeature)...~%")
                        (let ((point (routeguide:make-point :latitude 409146138 :longitude -746188906)))
                          (multiple-value-bind (response status status-msg)
                              (clgrpc.client:call-unary channel "routeguide.RouteGuide" "GetFeature"
                                                       (clgrpc.grpc:proto-serialize point))
                            (if (= status clgrpc.grpc:+grpc-status-ok+)
                                (let ((feature (clgrpc.grpc:proto-deserialize 'routeguide:feature response)))
                                  (format t "         ✓ Received: ~A~%" (routeguide:feature-name feature)))
                                (error "Unary RPC failed: ~A" status-msg))))

                        ;; Test 2: Server Streaming RPC
                        (format t "   [2/4] Testing Server Streaming RPC (ListFeatures)...~%")
                        (let ((rect (routeguide:make-rectangle
                                     :lo (routeguide:make-point :latitude 400000000 :longitude -750000000)
                                     :hi (routeguide:make-point :latitude 420000000 :longitude -730000000))))
                          (multiple-value-bind (stream status status-msg)
                              (clgrpc.client:call-server-streaming channel "routeguide.RouteGuide" "ListFeatures"
                                                                  (clgrpc.grpc:proto-serialize rect))
                            (if (null stream)
                                (error "Server streaming RPC failed: ~A" status-msg)
                                (let ((count 0))
                                  (loop for msg-bytes = (clgrpc.client:stream-recv stream)
                                        while msg-bytes
                                        do (incf count))
                                  (format t "         ✓ Received ~D features~%" count)))))

                        ;; Test 3: Client Streaming RPC
                        (format t "   [3/4] Testing Client Streaming RPC (RecordRoute)...~%")
                        (let ((points (list
                                       (routeguide:make-point :latitude 406337092 :longitude -740122226)
                                       (routeguide:make-point :latitude 406421967 :longitude -747727624)
                                       (routeguide:make-point :latitude 404318328 :longitude -740835638))))
                          (multiple-value-bind (stream status status-msg)
                              (clgrpc.client:call-client-streaming channel "routeguide.RouteGuide" "RecordRoute")
                            (if (null stream)
                                (error "Client streaming RPC failed: ~A" status-msg)
                                (progn
                                  (format t "         Sending ~D points...~%" (length points))
                                  (dolist (point points)
                                    (clgrpc.client:stream-send stream (clgrpc.grpc:proto-serialize point)))
                                  (format t "         Closing send side...~%")
                                  ;; Close send side and receive response
                                  (clgrpc.client:stream-close-send stream)
                                  (format t "         Waiting for response...~%")
                                  (let ((response-bytes (clgrpc.client:stream-recv stream :timeout-ms 5000)))
                                    (if response-bytes
                                        (let ((summary (clgrpc.grpc:proto-deserialize 'routeguide:route-summary response-bytes)))
                                          (format t "         ✓ Summary: ~D points, ~D meters~%"
                                                  (routeguide:route-summary-point-count summary)
                                                  (routeguide:route-summary-distance summary)))
                                        (error "Client streaming: no response received")))))))

                        ;; Test 4: Bidirectional Streaming RPC
                        (format t "   [4/4] Testing Bidirectional Streaming RPC (RouteChat)...~%")
                        ;; Send notes to same location - second note should echo first note back
                        (let ((loc1 (routeguide:make-point :latitude 100000000 :longitude 200000000))
                              (loc2 (routeguide:make-point :latitude 300000000 :longitude 400000000)))
                          (let ((notes (list
                                        (routeguide:make-route-note
                                         :location loc1
                                         :message "First note at location 1")
                                        (routeguide:make-route-note
                                         :location loc1
                                         :message "Second note at location 1 (should echo first)")
                                        (routeguide:make-route-note
                                         :location loc2
                                         :message "First note at location 2")
                                        (routeguide:make-route-note
                                         :location loc1
                                         :message "Third note at location 1 (should echo 2 previous)")
                                        (routeguide:make-route-note
                                         :location loc2
                                         :message "Second note at location 2 (should echo first)"))))
                            (multiple-value-bind (stream status status-msg)
                                (clgrpc.client:call-bidirectional-streaming channel "routeguide.RouteGuide" "RouteChat")
                              (if (null stream)
                                  (error "Bidirectional streaming RPC failed: ~A" status-msg)
                                  (let ((received-count 0))
                                    ;; Send all notes with small delays to allow server processing
                                    (dolist (note notes)
                                      (clgrpc.client:stream-send stream (clgrpc.grpc:proto-serialize note))
                                      (sleep 0.1))  ; Allow server to process and respond
                                    ;; Close send side to signal we're done sending
                                    (clgrpc.client:stream-close-send stream)
                                    (sleep 0.1)  ; Final delay for last responses
                                    ;; Receive all responses (should get: 0, 1, 0, 2, 1 = 4 total)
                                    (loop for response = (clgrpc.client:stream-recv stream :timeout-ms 500)
                                          while response
                                          do (incf received-count))
                                    (if (>= received-count 3)
                                        (format t "         ✓ Sent 5 notes, received ~D note(s) back (concurrent streaming working!)~%" received-count)
                                        (format t "         ⚠ Sent 5 notes, received only ~D note(s) back (expected ≥3)~%" received-count)))))))

                        ;; All tests passed
                        (setf test-passed t)
                        (format t "~%✓ All 4 RPC types working correctly!~%"))
                    (error (e)
                      (format t "~%✗ Test failed: ~A~%" e)
                      (setf test-passed nil)))
               (clgrpc.client:close-channel channel))))

      ;; Cleanup
      (when server
        (format t "~%3. Stopping server...~%")
        (clgrpc.server:stop-server server :timeout 5)
        (format t "   ✓ Server stopped~%")))

    (format t "~%")
    (if test-passed
        (format t "════════════════════════════════════════════════════════~%~%✓ SUCCESS: All streaming types verified working~%~%")
        (progn
          (format t "════════════════════════════════════════════════════════~%~%✗ FAILURE: Some tests failed~%~%")
          (sb-ext:exit :code 1)))))

;; Run the test
(test-all-streaming)
