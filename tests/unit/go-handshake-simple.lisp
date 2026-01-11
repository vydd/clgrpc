;;;; Simplified handshake test with detailed logging
;;;;
;;;; Prerequisites:
;;;;   (ql:quickload :clgrpc)
;;;;   Go server binary must be built: cd tests/interop && ./setup.sh
;;;; Usage: sbcl --load go-handshake-simple.lisp

(ql:quickload :clgrpc :silent t)

(format t "~%=== HTTP/2 Handshake Test ===~%")

(defvar *server-binary*
  (asdf:system-relative-pathname :clgrpc "tests/interop/bin/greeter_server"))

;; Start Go server
(format t "1. Starting Go server...~%")
(let ((server (uiop:launch-program
               (list (namestring *server-binary*))
               :output :stream :error-output :stream)))
  (sleep 1)
  (format t "   Server PID: ~A~%~%" (uiop:process-info-pid server))

  (unwind-protect
      (handler-case
          (progn
            ;; Connect
            (format t "2. Connecting...~%")
            (let* ((socket (clgrpc.transport:make-tcp-connection "localhost" 50051))
                   (buffered (clgrpc.transport:wrap-socket-with-buffer socket)))
              (format t "   ✓ Connected~%~%")

              (unwind-protect
                  (progn
                    ;; Send preface
                    (format t "3. Sending preface (24 bytes)...~%")
                    (let ((preface (clgrpc.http2:http2-client-preface-bytes)))
                      (clgrpc.transport:buffered-write-bytes buffered preface)
                      (clgrpc.transport:buffered-flush buffered)
                      (format t "   ✓ Sent~%~%"))

                    ;; Send SETTINGS
                    (format t "4. Sending empty SETTINGS frame (9 bytes)...~%")
                    (let* ((frame (clgrpc.http2:make-http2-frame
                                   :length 0
                                   :type 4  ; SETTINGS
                                   :flags 0
                                   :stream-id 0
                                   :payload (make-array 0 :element-type '(unsigned-byte 8))))
                           (encoded (clgrpc.http2:encode-frame frame)))
                      (clgrpc.transport:buffered-write-bytes buffered encoded)
                      (clgrpc.transport:buffered-flush buffered)
                      (format t "   ✓ Sent~%~%"))

                    ;; Try to read with timeout using wait-for-input
                    (format t "5. Attempting to read server response (with 2s timeout)...~%")
                    (if (clgrpc.transport:socket-wait-for-input socket :timeout 2)
                        (progn
                          (format t "   ✓ Data available!~%")
                          (let ((header (clgrpc.transport:buffered-read-bytes buffered 9)))
                            (format t "   Header: ~{~2,'0X ~}~%" (coerce header 'list))
                            (let ((type (aref header 3)))
                              (format t "   Frame type: ~D~%" type)
                              (if (= type 4)
                                  (format t "   ✓ SUCCESS: Received SETTINGS frame!~%")
                                  (format t "   ✗ Wrong frame type~%")))))
                        (format t "   ✗ TIMEOUT: No data received from server~%")))

                (clgrpc.transport:buffered-close buffered))))
        (error (e)
          (format t "~%ERROR: ~A~%" e)))

    ;; Cleanup
    (format t "~%6. Stopping server...~%")
    (uiop:terminate-process server)))

(format t "Done.~%")
