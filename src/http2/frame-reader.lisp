;;;; frame-reader.lisp - Reading HTTP/2 frames from buffered sockets

(in-package #:clgrpc.http2)

(defun read-bytes-from-stream (buffered-socket count)
  "Read exactly count bytes from buffered socket. Returns byte array or signals error."
  ;; Use buffered I/O from transport layer
  (clgrpc.transport:buffered-read-bytes buffered-socket count))

(defun read-frame-from-stream (buffered-socket)
  "Read a single HTTP/2 frame from buffered socket. Returns http2-frame or nil on EOF.

   Uses buffered-peek-bytes for efficient frame header reading."
  (handler-case
      (progn
        ;; Peek at frame header (9 bytes) without consuming
        ;; This is efficient - doesn't require multiple read calls
        (let ((header (clgrpc.transport:buffered-peek-bytes buffered-socket +frame-header-size+)))

          ;; Decode header to get payload length
          (multiple-value-bind (length type flags stream-id)
              (decode-frame-header header)

            ;; Now read the complete frame (header + payload)
            (let* ((total-size (+ +frame-header-size+ length))
                   (frame-bytes (clgrpc.transport:buffered-read-bytes buffered-socket total-size)))

              (format *error-output* "RECV: type=~D stream=~D len=~D flags=~D~%"
                      type stream-id length flags)

              ;; Extract payload (skip 9-byte header)
              (let ((payload (if (zerop length)
                                (make-byte-array 0)
                                (subseq frame-bytes +frame-header-size+))))

                ;; Construct frame
                (make-http2-frame :length length
                                 :type type
                                 :flags flags
                                 :stream-id stream-id
                                 :payload payload))))))
    (error (e)
      ;; EOF or read error - return nil
      (when (search "EOF" (format nil "~A" e))
        (return-from read-frame-from-stream nil))
      ;; Re-signal other errors
      (error e))))

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
