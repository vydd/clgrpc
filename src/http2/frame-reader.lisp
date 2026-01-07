;;;; frame-reader.lisp - Reading HTTP/2 frames from streams

(in-package #:clgrpc.http2)

(defun read-bytes-from-stream (stream count)
  "Read exactly count bytes from stream. Returns byte array or signals error."
  (let ((buffer (make-byte-array count)))
    (let ((bytes-read (read-sequence buffer stream)))
      (unless (= bytes-read count)
        (error "Unexpected end of stream: expected ~D bytes, got ~D"
               count bytes-read))
      buffer)))

(defun read-frame-from-stream (stream)
  "Read a single HTTP/2 frame from stream. Returns http2-frame or nil on EOF."
  ;; Read frame header (9 bytes)
  (handler-case
      (let ((header (read-bytes-from-stream stream +frame-header-size+)))
        ;; Decode header
        (multiple-value-bind (length type flags stream-id)
            (decode-frame-header header)

          ;; Read payload
          (let ((payload (if (zerop length)
                            (make-byte-array 0)
                            (read-bytes-from-stream stream length))))

            ;; Construct frame
            (make-http2-frame :length length
                             :type type
                             :flags flags
                             :stream-id stream-id
                             :payload payload))))
    (end-of-file ()
      nil)))

(defun read-continuation-frames (stream initial-frame)
  "Read CONTINUATION frames following a HEADERS frame.
   Returns list of all frames (including initial)."
  (let ((frames (list initial-frame)))
    (loop
      (when (frame-flag-set-p (first frames) +flag-end-headers+)
        (return))

      (let ((next-frame (read-frame-from-stream stream)))
        (unless next-frame
          (signal-protocol-error "Unexpected EOF while reading CONTINUATION frames"))

        (unless (= (frame-type next-frame) +frame-type-continuation+)
          (signal-protocol-error "Expected CONTINUATION frame, got type ~A"
                               (frame-type next-frame)))

        (unless (= (frame-stream-id next-frame)
                  (frame-stream-id (first frames)))
          (signal-protocol-error "CONTINUATION frame stream ID mismatch"))

        (push next-frame frames)))

    (nreverse frames)))

(defun combine-header-fragments (frames)
  "Combine header block fragments from multiple frames.
   Returns single byte array with complete header block."
  (let ((fragments (mapcar #'frame-payload frames)))
    (if (= (length fragments) 1)
        (first fragments)
        (let* ((total-size (reduce #'+ fragments :key #'length))
               (result (make-byte-array total-size))
               (offset 0))
          (dolist (fragment fragments)
            (copy-bytes fragment result :start2 offset)
            (incf offset (length fragment)))
          result))))
