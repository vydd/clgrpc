;;;; frames.lisp - HTTP/2 frame structures and encoding/decoding (RFC 9113)

(in-package #:clgrpc.http2)

;;; Frame Types (RFC 9113 Section 6)

(defconstant +frame-type-data+ #x00 "DATA frame")
(defconstant +frame-type-headers+ #x01 "HEADERS frame")
(defconstant +frame-type-priority+ #x02 "PRIORITY frame")
(defconstant +frame-type-rst-stream+ #x03 "RST_STREAM frame")
(defconstant +frame-type-settings+ #x04 "SETTINGS frame")
(defconstant +frame-type-push-promise+ #x05 "PUSH_PROMISE frame")
(defconstant +frame-type-ping+ #x06 "PING frame")
(defconstant +frame-type-goaway+ #x07 "GOAWAY frame")
(defconstant +frame-type-window-update+ #x08 "WINDOW_UPDATE frame")
(defconstant +frame-type-continuation+ #x09 "CONTINUATION frame")

;;; Frame Flags

(defconstant +flag-end-stream+ #x01 "END_STREAM flag")
(defconstant +flag-end-headers+ #x04 "END_HEADERS flag")
(defconstant +flag-padded+ #x08 "PADDED flag")
(defconstant +flag-priority+ #x20 "PRIORITY flag")
(defconstant +flag-ack+ #x01 "ACK flag (for SETTINGS and PING)")

;;; Frame Constants

(defconstant +frame-header-size+ 9
  "Size of HTTP/2 frame header in bytes")

(defconstant +default-max-frame-size+ 16384
  "Default maximum frame payload size (16 KB)")

(defconstant +max-frame-size-limit+ 16777215
  "Maximum allowed frame size (2^24 - 1 bytes)")

;;; Frame Structure

(defstruct (http2-frame (:conc-name frame-))
  "HTTP/2 frame structure

   Frame Format (RFC 9113 Section 4.1):
   +-----------------------------------------------+
   |                 Length (24)                   |
   +---------------+---------------+---------------+
   |   Type (8)    |   Flags (8)   |
   +-+-------------+---------------+-------------------------------+
   |R|                 Stream Identifier (31)                      |
   +=+=============================================================+
   |                   Frame Payload (0...)                      ...
   +---------------------------------------------------------------+"
  (length 0 :type (unsigned-byte 24)
          :read-only t)
  (type 0 :type (unsigned-byte 8)
        :read-only t)
  (flags 0 :type (unsigned-byte 8))
  (stream-id 0 :type (unsigned-byte 31)
             :read-only t)
  (payload #() :type (simple-array (unsigned-byte 8) (*))
           :read-only t))

;;; Frame Flag Utilities

(defun frame-flag-set-p (frame flag)
  "Check if a specific flag is set in the frame."
  (declare (type http2-frame frame)
           (type (unsigned-byte 8) flag))
  (not (zerop (logand (frame-flags frame) flag))))

(defun set-frame-flag (frame flag)
  "Set a specific flag in the frame."
  (declare (type http2-frame frame)
           (type (unsigned-byte 8) flag))
  (setf (frame-flags frame) (logior (frame-flags frame) flag)))

(defun clear-frame-flag (frame flag)
  "Clear a specific flag in the frame."
  (declare (type http2-frame frame)
           (type (unsigned-byte 8) flag))
  (setf (frame-flags frame) (logand (frame-flags frame) (lognot flag))))

;;; Frame Header Encoding/Decoding

(defun encode-frame-header (frame)
  "Encode HTTP/2 frame header (9 bytes)."
  (declare (type http2-frame frame))
  (let ((header (make-byte-array +frame-header-size+)))
    ;; Length (24 bits)
    (encode-uint24-be (frame-length frame) header 0)
    ;; Type (8 bits)
    (setf (aref header 3) (frame-type frame))
    ;; Flags (8 bits)
    (setf (aref header 4) (frame-flags frame))
    ;; Stream ID (31 bits, R bit must be 0)
    (encode-uint32-be (logand (frame-stream-id frame) #x7FFFFFFF) header 5)
    header))

(defun decode-frame-header (bytes &optional (offset 0))
  "Decode HTTP/2 frame header from byte array.
   Returns: (values length type flags stream-id)"
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum offset))
  (when (< (- (length bytes) offset) +frame-header-size+)
    (error "Not enough bytes for frame header"))
  (let ((length (decode-uint24-be bytes offset))
        (type (aref bytes (+ offset 3)))
        (flags (aref bytes (+ offset 4)))
        (stream-id (logand (decode-uint32-be bytes (+ offset 5)) #x7FFFFFFF)))
    (values length type flags stream-id)))

;;; Frame Encoding

(defun encode-frame (frame)
  "Encode a complete HTTP/2 frame (header + payload)."
  (declare (type http2-frame frame))
  (let* ((header (encode-frame-header frame))
         (payload (frame-payload frame))
         (total-size (+ +frame-header-size+ (length payload)))
         (result (make-byte-array total-size)))
    (copy-bytes header result :start2 0)
    (copy-bytes payload result :start2 +frame-header-size+)
    result))

;;; Frame Decoding

(defun decode-frame (bytes &optional (offset 0))
  "Decode a complete HTTP/2 frame from byte array."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum offset))
  (multiple-value-bind (length type flags stream-id)
      (decode-frame-header bytes offset)
    (when (< (- (length bytes) offset) (+ +frame-header-size+ length))
      (error "Not enough bytes for frame payload"))
    (let ((payload (make-byte-array length)))
      (copy-bytes bytes payload
                  :start1 (+ offset +frame-header-size+)
                  :end1 (+ offset +frame-header-size+ length))
      (make-http2-frame :length length
                        :type type
                        :flags flags
                        :stream-id stream-id
                        :payload payload))))

;;; Specific Frame Constructors

(defun make-data-frame (stream-id data &key end-stream padded padding-length)
  "Create a DATA frame (RFC 9113 Section 6.1)."
  (declare (type (unsigned-byte 31) stream-id)
           (type (simple-array (unsigned-byte 8) (*)) data))
  (when (zerop stream-id)
    (signal-protocol-error "DATA frame must have non-zero stream ID"))

  (let* ((pad-len (if padded (or padding-length 0) 0))
         (payload-size (+ (if padded 1 0) (length data) pad-len))
         (payload (make-byte-array payload-size))
         (flags 0))

    ;; Set flags
    (when end-stream
      (setf flags (logior flags +flag-end-stream+)))
    (when padded
      (setf flags (logior flags +flag-padded+)))

    ;; Build payload
    (let ((pos 0))
      (when padded
        (setf (aref payload pos) pad-len)
        (incf pos))
      (copy-bytes data payload :start2 pos)
      (incf pos (length data))
      ;; Padding bytes are already zero from make-byte-array
      )

    (make-http2-frame :length payload-size
                      :type +frame-type-data+
                      :flags flags
                      :stream-id stream-id
                      :payload payload)))

(defun make-headers-frame (stream-id header-block-fragment
                           &key end-stream end-headers padded padding-length
                                priority exclusive stream-dependency weight)
  "Create a HEADERS frame (RFC 9113 Section 6.2)."
  (declare (type (unsigned-byte 31) stream-id)
           (type (simple-array (unsigned-byte 8) (*)) header-block-fragment))
  (when (zerop stream-id)
    (signal-protocol-error "HEADERS frame must have non-zero stream ID"))

  (let* ((pad-len (if padded (or padding-length 0) 0))
         (has-priority (and priority stream-dependency))
         (payload-size (+ (if padded 1 0)
                         (if has-priority 5 0)
                         (length header-block-fragment)
                         pad-len))
         (payload (make-byte-array payload-size))
         (flags 0))

    ;; Set flags
    (when end-stream
      (setf flags (logior flags +flag-end-stream+)))
    (when end-headers
      (setf flags (logior flags +flag-end-headers+)))
    (when padded
      (setf flags (logior flags +flag-padded+)))
    (when has-priority
      (setf flags (logior flags +flag-priority+)))

    ;; Build payload
    (let ((pos 0))
      (when padded
        (setf (aref payload pos) pad-len)
        (incf pos))

      (when has-priority
        (let ((dep-val (if exclusive
                          (logior stream-dependency #x80000000)
                          stream-dependency)))
          (encode-uint32-be dep-val payload pos))
        (incf pos 4)
        (setf (aref payload pos) (or weight 0))
        (incf pos))

      (copy-bytes header-block-fragment payload :start2 pos))

    (make-http2-frame :length payload-size
                      :type +frame-type-headers+
                      :flags flags
                      :stream-id stream-id
                      :payload payload)))

(defun make-priority-frame (stream-id stream-dependency &key exclusive (weight 0))
  "Create a PRIORITY frame (RFC 9113 Section 6.3)."
  (declare (type (unsigned-byte 31) stream-id stream-dependency)
           (type (unsigned-byte 8) weight))
  (when (zerop stream-id)
    (signal-protocol-error "PRIORITY frame must have non-zero stream ID"))
  (when (= stream-id stream-dependency)
    (signal-protocol-error "Stream cannot depend on itself"))

  (let ((payload (make-byte-array 5)))
    (let ((dep-val (if exclusive
                      (logior stream-dependency #x80000000)
                      stream-dependency)))
      (encode-uint32-be dep-val payload 0))
    (setf (aref payload 4) weight)

    (make-http2-frame :length 5
                      :type +frame-type-priority+
                      :flags 0
                      :stream-id stream-id
                      :payload payload)))

(defun make-rst-stream-frame (stream-id error-code)
  "Create a RST_STREAM frame (RFC 9113 Section 6.4)."
  (declare (type (unsigned-byte 31) stream-id)
           (type (unsigned-byte 32) error-code))
  (when (zerop stream-id)
    (signal-protocol-error "RST_STREAM frame must have non-zero stream ID"))

  (let ((payload (make-byte-array 4)))
    (encode-uint32-be error-code payload 0)

    (make-http2-frame :length 4
                      :type +frame-type-rst-stream+
                      :flags 0
                      :stream-id stream-id
                      :payload payload)))

(defun make-settings-frame (&key ack settings)
  "Create a SETTINGS frame (RFC 9113 Section 6.5).
   Settings is an alist of (setting-id . value) pairs."
  (when (and ack settings)
    (signal-protocol-error "SETTINGS frame with ACK must not have payload"))

  (let* ((payload-size (if ack 0 (* 6 (length settings))))
         (payload (make-byte-array payload-size))
         (flags (if ack +flag-ack+ 0)))

    (unless ack
      (loop for (id . value) in settings
            for pos from 0 by 6
            do (encode-uint16-be id payload pos)
               (encode-uint32-be value payload (+ pos 2))))

    (make-http2-frame :length payload-size
                      :type +frame-type-settings+
                      :flags flags
                      :stream-id 0
                      :payload payload)))

(defun make-push-promise-frame (stream-id promised-stream-id header-block-fragment
                               &key end-headers padded padding-length)
  "Create a PUSH_PROMISE frame (RFC 9113 Section 6.6)."
  (declare (type (unsigned-byte 31) stream-id promised-stream-id)
           (type (simple-array (unsigned-byte 8) (*)) header-block-fragment))
  (when (zerop stream-id)
    (signal-protocol-error "PUSH_PROMISE frame must have non-zero stream ID"))

  (let* ((pad-len (if padded (or padding-length 0) 0))
         (payload-size (+ (if padded 1 0) 4 (length header-block-fragment) pad-len))
         (payload (make-byte-array payload-size))
         (flags 0))

    (when end-headers
      (setf flags (logior flags +flag-end-headers+)))
    (when padded
      (setf flags (logior flags +flag-padded+)))

    (let ((pos 0))
      (when padded
        (setf (aref payload pos) pad-len)
        (incf pos))

      (encode-uint32-be (logand promised-stream-id #x7FFFFFFF) payload pos)
      (incf pos 4)

      (copy-bytes header-block-fragment payload :start2 pos))

    (make-http2-frame :length payload-size
                      :type +frame-type-push-promise+
                      :flags flags
                      :stream-id stream-id
                      :payload payload)))

(defun make-ping-frame (opaque-data &key ack)
  "Create a PING frame (RFC 9113 Section 6.7).
   Opaque-data must be exactly 8 bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) opaque-data))
  (unless (= (length opaque-data) 8)
    (signal-protocol-error "PING payload must be exactly 8 bytes"))

  (let ((flags (if ack +flag-ack+ 0)))
    (make-http2-frame :length 8
                      :type +frame-type-ping+
                      :flags flags
                      :stream-id 0
                      :payload opaque-data)))

(defun make-goaway-frame (last-stream-id error-code &optional additional-debug-data)
  "Create a GOAWAY frame (RFC 9113 Section 6.8)."
  (declare (type (unsigned-byte 31) last-stream-id)
           (type (unsigned-byte 32) error-code))

  (let* ((debug-data (or additional-debug-data #()))
         (payload-size (+ 8 (length debug-data)))
         (payload (make-byte-array payload-size)))

    (encode-uint32-be (logand last-stream-id #x7FFFFFFF) payload 0)
    (encode-uint32-be error-code payload 4)
    (when (plusp (length debug-data))
      (copy-bytes debug-data payload :start2 8))

    (make-http2-frame :length payload-size
                      :type +frame-type-goaway+
                      :flags 0
                      :stream-id 0
                      :payload payload)))

(defun make-window-update-frame (stream-id window-size-increment)
  "Create a WINDOW_UPDATE frame (RFC 9113 Section 6.9)."
  (declare (type (unsigned-byte 31) stream-id window-size-increment))
  (when (zerop window-size-increment)
    (signal-protocol-error "WINDOW_UPDATE increment must be non-zero"))
  (when (> window-size-increment #x7FFFFFFF)
    (signal-protocol-error "WINDOW_UPDATE increment too large"))

  (let ((payload (make-byte-array 4)))
    (encode-uint32-be (logand window-size-increment #x7FFFFFFF) payload 0)

    (make-http2-frame :length 4
                      :type +frame-type-window-update+
                      :flags 0
                      :stream-id stream-id
                      :payload payload)))

(defun make-continuation-frame (stream-id header-block-fragment &key end-headers)
  "Create a CONTINUATION frame (RFC 9113 Section 6.10)."
  (declare (type (unsigned-byte 31) stream-id)
           (type (simple-array (unsigned-byte 8) (*)) header-block-fragment))
  (when (zerop stream-id)
    (signal-protocol-error "CONTINUATION frame must have non-zero stream ID"))

  (let ((flags (if end-headers +flag-end-headers+ 0)))
    (make-http2-frame :length (length header-block-fragment)
                      :type +frame-type-continuation+
                      :flags flags
                      :stream-id stream-id
                      :payload header-block-fragment)))
