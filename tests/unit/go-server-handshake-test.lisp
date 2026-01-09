;;;; Unit test: HTTP/2 handshake with Go server

(require :asdf)
(push #P"/home/vydd/Code/clgrpc/" asdf:*central-registry*)
(asdf:load-system :clgrpc :verbose nil)

(format t "~%Testing HTTP/2 handshake with Go gRPC server...~%")

;; Start Go server as subprocess
(format t "1. Starting Go server...~%")
(let ((server-process (uiop:launch-program
                       '("/home/vydd/Code/clgrpc/tests/interop/bin/greeter_server")
                       :output :stream
                       :error-output :stream)))
  (sleep 1)  ; Give server time to start
  (format t "   ✓ Go server started (PID: ~A)~%" (uiop:process-info-pid server-process))

  (unwind-protect
      (progn
        ;; Connect to server
        (format t "2. Connecting to localhost:50051...~%")
        (let* ((socket (clgrpc.transport:make-tcp-connection "localhost" 50051))
               (buffered (clgrpc.transport:wrap-socket-with-buffer socket)))
          (format t "   ✓ Connected~%")

          (unwind-protect
              (progn
                ;; Send preface
                (format t "3. Sending HTTP/2 preface...~%")
                (let ((preface (clgrpc.http2:http2-client-preface-bytes)))
                  (clgrpc.transport:buffered-write-bytes buffered preface)
                  (clgrpc.transport:buffered-flush buffered)
                  (format t "   ✓ Sent ~D bytes: ~{~2,'0X ~}~%"
                          (length preface) (coerce preface 'list)))

                ;; Send empty SETTINGS frame
                (format t "4. Sending SETTINGS frame...~%")
                (let* ((frame (clgrpc.http2:make-http2-frame
                               :length 0
                               :type clgrpc.http2:+frame-type-settings+
                               :flags 0
                               :stream-id 0
                               :payload (make-array 0 :element-type '(unsigned-byte 8))))
                       (encoded (clgrpc.http2:encode-frame frame)))
                  (clgrpc.transport:buffered-write-bytes buffered encoded)
                  (clgrpc.transport:buffered-flush buffered)
                  (format t "   ✓ Sent SETTINGS: ~{~2,'0X ~}~%" (coerce encoded 'list)))

                ;; Try to read server SETTINGS (with timeout)
                (format t "5. Reading server SETTINGS frame...~%")
                (handler-case
                    (let ((header (clgrpc.transport:buffered-read-bytes buffered 9)))
                      (format t "   ✓ Received frame header: ~{~2,'0X ~}~%" (coerce header 'list))

                      ;; Decode frame header
                      (let* ((length (logior (ash (aref header 0) 16)
                                            (ash (aref header 1) 8)
                                            (aref header 2)))
                             (type (aref header 3))
                             (flags (aref header 4))
                             (stream-id (logior (ash (logand (aref header 5) #x7F) 24)
                                               (ash (aref header 6) 16)
                                               (ash (aref header 7) 8)
                                               (aref header 8))))
                        (format t "   Frame: type=~D len=~D flags=~D stream=~D~%"
                                type length flags stream-id)

                        (assert (= type 4) () "Expected SETTINGS frame (type 4)")
                        (assert (= stream-id 0) () "SETTINGS must be on stream 0")

                        ;; Read payload if any
                        (when (> length 0)
                          (let ((payload (clgrpc.transport:buffered-read-bytes buffered length)))
                            (format t "   Payload: ~{~2,'0X ~}~%" (coerce payload 'list))))

                        (format t "   ✓ Valid SETTINGS frame received!~%")))
                  (error (e)
                    (format t "   ✗ Error reading: ~A~%" e))))

            (clgrpc.transport:buffered-close buffered))))

    ;; Kill server
    (format t "6. Stopping Go server...~%")
    (uiop:terminate-process server-process)
    (sleep 0.5)))

(format t "~%✓ Handshake test complete~%")
