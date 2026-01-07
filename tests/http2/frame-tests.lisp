;;;; frame-tests.lisp - Comprehensive tests for HTTP/2 frames

(in-package #:clgrpc-tests)

(def-suite frame-tests
  :in http2-tests
  :description "HTTP/2 frame encoding/decoding tests")

(in-suite frame-tests)

;;; Frame Header Tests

(test frame-header-encode-decode
  "Test frame header encoding and decoding"
  (let* ((frame (make-http2-frame :length 42
                                  :type +frame-type-data+
                                  :flags #x01
                                  :stream-id 123
                                  :payload (make-byte-array 0)))
         (encoded (encode-frame-header frame)))

    (is (= (length encoded) clgrpc.http2:+frame-header-size+))

    (multiple-value-bind (length type flags stream-id)
        (decode-frame-header encoded)
      (is (= length 42))
      (is (= type +frame-type-data+))
      (is (= flags #x01))
      (is (= stream-id 123)))))

(test frame-header-stream-id-r-bit
  "Test that R bit is masked out in stream ID"
  (let* ((frame (make-http2-frame :length 0
                                  :type +frame-type-data+
                                  :flags 0
                                  :stream-id #x7FFFFFFF  ; Max valid stream ID
                                  :payload (make-byte-array 0)))
         (encoded (encode-frame-header frame)))

    (multiple-value-bind (length type flags stream-id)
        (decode-frame-header encoded)
      (declare (ignore length type flags))
      (is (= stream-id #x7FFFFFFF))
      ;; Verify R bit is 0
      (is (zerop (logand (decode-uint32-be encoded 5) #x80000000))))))

;;; DATA Frame Tests

(test data-frame-basic
  "Test basic DATA frame creation and encoding"
  (let* ((payload (make-byte-array 5 :initial-element 1))
         (frame (make-data-frame 1 payload))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (= (frame-type decoded) +frame-type-data+))
    (is (= (frame-stream-id decoded) 1))
    (is (= (frame-length decoded) 5))
    (is (equalp (frame-payload decoded) payload))))

(test data-frame-end-stream
  "Test DATA frame with END_STREAM flag"
  (let* ((payload (bytes 1 2 3))
         (frame (make-data-frame 1 payload :end-stream t)))

    (is (frame-flag-set-p frame +flag-end-stream+))

    (let ((decoded (decode-frame (encode-frame frame))))
      (is (frame-flag-set-p decoded +flag-end-stream+)))))

(test data-frame-padded
  "Test DATA frame with padding"
  (let* ((payload (make-byte-array 5 :initial-element 1))
         (frame (make-data-frame 1 payload :padded t :padding-length 10))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (frame-flag-set-p frame +flag-padded+))
    (is (= (frame-length decoded) (+ 1 5 10)))  ; pad length + data + padding
    (is (= (aref (frame-payload decoded) 0) 10))  ; First byte is pad length
    ))

(test data-frame-zero-stream-id-error
  "Test that DATA frame with stream ID 0 signals error"
  (signals http2-protocol-error
    (make-data-frame 0 (bytes 1 2 3))))

;;; HEADERS Frame Tests

(test headers-frame-basic
  "Test basic HEADERS frame"
  (let* ((header-block (bytes 10 20 30 40))
         (frame (make-headers-frame 1 header-block :end-headers t))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (= (frame-type decoded) +frame-type-headers+))
    (is (= (frame-stream-id decoded) 1))
    (is (frame-flag-set-p decoded +flag-end-headers+))
    (is (equalp (frame-payload decoded) header-block))))

(test headers-frame-with-priority
  "Test HEADERS frame with priority information"
  (let* ((header-block (bytes 10 20 30))
         (frame (make-headers-frame 1 header-block
                                   :end-headers t
                                   :priority t
                                   :stream-dependency 0
                                   :exclusive t
                                   :weight 100))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (frame-flag-set-p decoded +flag-priority+))
    (is (= (frame-length decoded) (+ 5 3)))  ; 5 bytes priority + header block

    ;; First 4 bytes are stream dependency with E bit
    (let ((dep (decode-uint32-be (frame-payload decoded) 0)))
      (is (not (zerop (logand dep #x80000000)))))  ; E bit set

    ;; 5th byte is weight
    (is (= (aref (frame-payload decoded) 4) 100))))

(test headers-frame-all-flags
  "Test HEADERS frame with all flags"
  (let* ((header-block (bytes 1 2 3))
         (frame (make-headers-frame 1 header-block
                                   :end-stream t
                                   :end-headers t
                                   :padded t
                                   :padding-length 5
                                   :priority t
                                   :stream-dependency 0
                                   :weight 50)))

    (is (frame-flag-set-p frame +flag-end-stream+))
    (is (frame-flag-set-p frame +flag-end-headers+))
    (is (frame-flag-set-p frame +flag-padded+))
    (is (frame-flag-set-p frame +flag-priority+))))

(test headers-frame-zero-stream-id-error
  "Test that HEADERS frame with stream ID 0 signals error"
  (signals http2-protocol-error
    (make-headers-frame 0 (bytes 1 2 3) :end-headers t)))

;;; PRIORITY Frame Tests

(test priority-frame-basic
  "Test basic PRIORITY frame"
  (let* ((frame (make-priority-frame 1 0 :weight 100))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (= (frame-type decoded) +frame-type-priority+))
    (is (= (frame-stream-id decoded) 1))
    (is (= (frame-length decoded) 5))

    (let ((payload (frame-payload decoded)))
      (is (= (aref payload 4) 100)))))

(test priority-frame-exclusive
  "Test PRIORITY frame with exclusive flag"
  (let* ((frame (make-priority-frame 1 10 :exclusive t :weight 200))
         (payload (frame-payload frame)))

    ;; First 4 bytes contain stream dependency with E bit
    (let ((dep (decode-uint32-be payload 0)))
      (is (not (zerop (logand dep #x80000000))))  ; E bit set
      (is (= (logand dep #x7FFFFFFF) 10)))))  ; Stream dependency is 10

(test priority-frame-self-dependency-error
  "Test that PRIORITY frame with self-dependency signals error"
  (signals http2-protocol-error
    (make-priority-frame 5 5 :weight 100)))

;;; RST_STREAM Frame Tests

(test rst-stream-frame-basic
  "Test basic RST_STREAM frame"
  (let* ((frame (make-rst-stream-frame 1 +http2-error-cancel+))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (= (frame-type decoded) +frame-type-rst-stream+))
    (is (= (frame-stream-id decoded) 1))
    (is (= (frame-length decoded) 4))

    (let ((error-code (decode-uint32-be (frame-payload decoded) 0)))
      (is (= error-code +http2-error-cancel+)))))

(test rst-stream-frame-zero-stream-id-error
  "Test that RST_STREAM with stream ID 0 signals error"
  (signals http2-protocol-error
    (make-rst-stream-frame 0 +http2-error-cancel+)))

;;; SETTINGS Frame Tests

(test settings-frame-empty
  "Test empty SETTINGS frame (initial)"
  (let* ((frame (make-settings-frame))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (= (frame-type decoded) +frame-type-settings+))
    (is (= (frame-stream-id decoded) 0))
    (is (= (frame-length decoded) 0))))

(test settings-frame-with-parameters
  "Test SETTINGS frame with parameters"
  (let* ((settings '((1 . 4096) (3 . 100) (4 . 65535)))
         (frame (make-settings-frame :settings settings))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (= (frame-length decoded) (* 6 3)))  ; 6 bytes per setting

    (let ((payload (frame-payload decoded)))
      ;; First setting: ID=1, Value=4096
      (is (= (decode-uint16-be payload 0) 1))
      (is (= (decode-uint32-be payload 2) 4096))

      ;; Second setting: ID=3, Value=100
      (is (= (decode-uint16-be payload 6) 3))
      (is (= (decode-uint32-be payload 8) 100)))))

(test settings-frame-ack
  "Test SETTINGS frame with ACK flag"
  (let* ((frame (make-settings-frame :ack t))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (frame-flag-set-p decoded +flag-ack+))
    (is (= (frame-length decoded) 0))))

(test settings-frame-ack-with-payload-error
  "Test that SETTINGS ACK with payload signals error"
  (signals http2-protocol-error
    (make-settings-frame :ack t :settings '((1 . 100)))))

;;; PING Frame Tests

(test ping-frame-basic
  "Test basic PING frame"
  (let* ((opaque-data (bytes 1 2 3 4 5 6 7 8))
         (frame (make-ping-frame opaque-data))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (= (frame-type decoded) +frame-type-ping+))
    (is (= (frame-stream-id decoded) 0))
    (is (= (frame-length decoded) 8))
    (is (equalp (frame-payload decoded) opaque-data))))

(test ping-frame-ack
  "Test PING frame with ACK flag"
  (let* ((opaque-data (bytes 1 2 3 4 5 6 7 8))
         (frame (make-ping-frame opaque-data :ack t)))

    (is (frame-flag-set-p frame +flag-ack+))))

(test ping-frame-wrong-size-error
  "Test that PING frame with wrong payload size signals error"
  (signals http2-protocol-error
    (make-ping-frame (bytes 1 2 3 4)))  ; Only 4 bytes
  (signals http2-protocol-error
    (make-ping-frame (bytes 1 2 3 4 5 6 7 8 9))))  ; 9 bytes

;;; GOAWAY Frame Tests

(test goaway-frame-basic
  "Test basic GOAWAY frame"
  (let* ((frame (make-goaway-frame 100 +http2-error-protocol-error+))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (= (frame-type decoded) +frame-type-goaway+))
    (is (= (frame-stream-id decoded) 0))
    (is (= (frame-length decoded) 8))

    (let ((payload (frame-payload decoded)))
      (is (= (decode-uint32-be payload 0) 100))
      (is (= (decode-uint32-be payload 4) +http2-error-protocol-error+)))))

(test goaway-frame-with-debug-data
  "Test GOAWAY frame with additional debug data"
  (let* ((debug-data (babel:string-to-octets "Connection closed"))
         (frame (make-goaway-frame 50 +http2-error-no-error+ debug-data))
         (payload (frame-payload frame)))

    (is (= (frame-length frame) (+ 8 (length debug-data))))

    ;; Verify debug data is appended after last-stream-id and error-code
    (let ((extracted-debug (subseq payload 8)))
      (is (equalp extracted-debug debug-data)))))

;;; WINDOW_UPDATE Frame Tests

(test window-update-frame-basic
  "Test basic WINDOW_UPDATE frame"
  (let* ((frame (make-window-update-frame 1 1000))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (= (frame-type decoded) +frame-type-window-update+))
    (is (= (frame-stream-id decoded) 1))
    (is (= (frame-length decoded) 4))

    (let ((increment (decode-uint32-be (frame-payload decoded) 0)))
      (is (= increment 1000)))))

(test window-update-frame-connection-level
  "Test WINDOW_UPDATE frame at connection level (stream ID 0)"
  (let* ((frame (make-window-update-frame 0 65536))
         (decoded (decode-frame (encode-frame frame))))

    (is (= (frame-stream-id decoded) 0))))

(test window-update-frame-zero-increment-error
  "Test that WINDOW_UPDATE with zero increment signals error"
  (signals http2-protocol-error
    (make-window-update-frame 1 0)))

(test window-update-frame-r-bit-masked
  "Test that R bit is masked in WINDOW_UPDATE"
  (let* ((frame (make-window-update-frame 1 #x7FFFFFFF))  ; Max increment
         (payload (frame-payload frame)))

    ;; Verify R bit is 0
    (is (zerop (logand (decode-uint32-be payload 0) #x80000000)))))

;;; CONTINUATION Frame Tests

(test continuation-frame-basic
  "Test basic CONTINUATION frame"
  (let* ((header-block (bytes 10 20 30 40 50))
         (frame (make-continuation-frame 1 header-block :end-headers t))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (= (frame-type decoded) +frame-type-continuation+))
    (is (= (frame-stream-id decoded) 1))
    (is (frame-flag-set-p decoded +flag-end-headers+))
    (is (equalp (frame-payload decoded) header-block))))

(test continuation-frame-zero-stream-id-error
  "Test that CONTINUATION with stream ID 0 signals error"
  (signals http2-protocol-error
    (make-continuation-frame 0 (bytes 1 2 3))))

;;; Frame Round-trip Tests

(test all-frames-round-trip
  "Test that all frame types can be encoded and decoded"
  (let ((frames (list
                 (make-data-frame 1 (bytes 1 2 3) :end-stream t)
                 (make-headers-frame 1 (bytes 10 20) :end-headers t)
                 (make-priority-frame 1 0 :weight 100)
                 (make-rst-stream-frame 1 +http2-error-cancel+)
                 (make-settings-frame :settings '((1 . 4096)))
                 (make-settings-frame :ack t)
                 (make-ping-frame (bytes 1 2 3 4 5 6 7 8))
                 (make-goaway-frame 100 +http2-error-no-error+)
                 (make-window-update-frame 1 1000)
                 (make-continuation-frame 1 (bytes 5 10 15) :end-headers t))))

    (dolist (frame frames)
      (let* ((encoded (encode-frame frame))
             (decoded (decode-frame encoded)))
        (is (= (frame-type frame) (frame-type decoded)))
        (is (= (frame-flags frame) (frame-flags decoded)))
        (is (= (frame-stream-id frame) (frame-stream-id decoded)))
        (is (= (frame-length frame) (frame-length decoded)))
        (is (equalp (frame-payload frame) (frame-payload decoded)))))))

;;; Edge Case Tests

(test frame-max-payload-size
  "Test frame with maximum payload size"
  (let* ((large-payload (make-byte-array clgrpc.http2:+max-frame-size-limit+))
         (frame (make-data-frame 1 large-payload))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (= (frame-length decoded) clgrpc.http2:+max-frame-size-limit+))
    (is (= (length (frame-payload decoded)) clgrpc.http2:+max-frame-size-limit+))))

(test frame-empty-payload
  "Test frame with empty payload"
  (let* ((frame (make-data-frame 1 (make-byte-array 0)))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))

    (is (= (frame-length decoded) 0))
    (is (= (length (frame-payload decoded)) 0))))
