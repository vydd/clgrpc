;;;; Unit test: SETTINGS frame encoding

(require :asdf)
(push #P"/home/vydd/Code/clgrpc/" asdf:*central-registry*)
(asdf:load-system :clgrpc :verbose nil)

(format t "~%Testing SETTINGS frame encoding...~%")

;; Test 1: Empty SETTINGS frame
(let* ((frame (clgrpc.http2:make-http2-frame
               :length 0
               :type clgrpc.http2:+frame-type-settings+
               :flags 0
               :stream-id 0
               :payload (make-array 0 :element-type '(unsigned-byte 8))))
       (encoded (clgrpc.http2:encode-frame frame)))
  (format t "1. Empty SETTINGS frame: ~D bytes~%" (length encoded))
  (format t "   Bytes: ~{~2,'0X ~}~%" (coerce encoded 'list))
  ;; Frame header: 9 bytes (3 length + 1 type + 1 flags + 4 stream-id)
  (assert (= (length encoded) 9) () "Empty SETTINGS should be 9 bytes")
  ;; Check frame type is 4 (SETTINGS)
  (assert (= (aref encoded 3) 4) () "Frame type should be 4 (SETTINGS)")
  (format t "   ✓ Valid empty SETTINGS frame~%"))

;; Test 2: SETTINGS frame structure
(let ((encoded (clgrpc.http2:encode-frame
                (clgrpc.http2:make-http2-frame
                 :length 0
                 :type clgrpc.http2:+frame-type-settings+
                 :flags 0
                 :stream-id 0
                 :payload (make-array 0 :element-type '(unsigned-byte 8))))))
  ;; Length field (3 bytes, big-endian, value = 0)
  (assert (= (aref encoded 0) 0) ())
  (assert (= (aref encoded 1) 0) ())
  (assert (= (aref encoded 2) 0) ())
  ;; Type (1 byte, value = 4)
  (assert (= (aref encoded 3) 4) ())
  ;; Flags (1 byte, value = 0)
  (assert (= (aref encoded 4) 0) ())
  ;; Stream ID (4 bytes, big-endian, value = 0)
  (assert (= (aref encoded 5) 0) ())
  (assert (= (aref encoded 6) 0) ())
  (assert (= (aref encoded 7) 0) ())
  (assert (= (aref encoded 8) 0) ())
  (format t "2. Frame structure: ✓ All fields correct~%"))

(format t "~%✓ All SETTINGS frame tests passed~%")
