;;;; frame-writer.lisp - Writing HTTP/2 frames to streams

(in-package #:clgrpc.http2)

(defun write-frame-to-stream (frame stream)
  "Write HTTP/2 frame to stream"
  (let ((encoded (encode-frame frame)))
    (write-sequence encoded stream)
    (force-output stream)))

(defun write-frames-to-stream (frames stream)
  "Write multiple HTTP/2 frames to stream"
  (dolist (frame frames)
    (write-frame-to-stream frame stream)))

(defun split-data-into-frames (stream-id data max-frame-size &key end-stream)
  "Split data into multiple DATA frames respecting max frame size.
   Returns list of DATA frames."
  (let ((frames nil)
        (offset 0)
        (remaining (length data)))

    (loop while (plusp remaining)
          do (let* ((chunk-size (min remaining max-frame-size))
                   (is-last (= (+ offset chunk-size) (length data)))
                   (chunk (subseq data offset (+ offset chunk-size)))
                   (frame (make-data-frame stream-id chunk
                                          :end-stream (and end-stream is-last))))
               (push frame frames)
               (incf offset chunk-size)
               (decf remaining chunk-size)))

    (nreverse frames)))

(defun split-headers-into-frames (stream-id header-block max-frame-size
                                  &key end-stream end-headers priority
                                      stream-dependency weight exclusive)
  "Split header block into HEADERS + CONTINUATION frames.
   Returns list of frames."
  (let ((frames nil)
        (offset 0)
        (remaining (length header-block)))

    ;; First frame is HEADERS
    (let* ((first-chunk-size (min remaining max-frame-size))
           (first-chunk (subseq header-block offset first-chunk-size))
           (is-complete (= first-chunk-size remaining))
           (first-frame (make-headers-frame stream-id first-chunk
                                           :end-stream end-stream
                                           :end-headers (and end-headers is-complete)
                                           :priority priority
                                           :stream-dependency stream-dependency
                                           :weight weight
                                           :exclusive exclusive)))
      (push first-frame frames)
      (incf offset first-chunk-size)
      (decf remaining first-chunk-size))

    ;; Subsequent frames are CONTINUATION
    (loop while (plusp remaining)
          do (let* ((chunk-size (min remaining max-frame-size))
                   (is-last (= (+ offset chunk-size) (length header-block)))
                   (chunk (subseq header-block offset (+ offset chunk-size)))
                   (frame (make-continuation-frame stream-id chunk
                                                  :end-headers (and end-headers is-last))))
               (push frame frames)
               (incf offset chunk-size)
               (decf remaining chunk-size)))

    (nreverse frames)))
