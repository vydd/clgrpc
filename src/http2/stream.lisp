;;;; stream.lisp - HTTP/2 stream state machine (RFC 9113 Section 5.1)

(in-package #:clgrpc.http2)

;;; Stream States (RFC 9113 Section 5.1)
;;;
;;; State diagram:
;;;                            +--------+
;;;                    send PP |        | recv PP
;;;                   ,--------|  idle  |--------.
;;;                  /         |        |         \
;;;                 v          +--------+          v
;;;          +----------+          |           +----------+
;;;          |          |          | send H /  |          |
;;;   ,------| reserved |          | recv H    | reserved |------.
;;;   |      | (local)  |          |           | (remote) |      |
;;;   |      +----------+          v           +----------+      |
;;;   |          |             +--------+             |          |
;;;   |          |     recv ES |        | send ES     |          |
;;;   |   send H |     ,-------|  open  |-------.     | recv H   |
;;;   |          |    /        |        |        \    |          |
;;;   |          v   v         +--------+         v   v          |
;;;   |      +----------+          |           +----------+      |
;;;   |      |   half   |          |           |   half   |      |
;;;   |      |  closed  |          | send R /  |  closed  |      |
;;;   |      | (remote) |          | recv R    | (local)  |      |
;;;   |      +----------+          |           +----------+      |
;;;   |           |                |                 |           |
;;;   |           | send ES /      |       recv ES / |           |
;;;   |           | send R /       v        send R / |           |
;;;   |           | recv R     +--------+   recv R   |           |
;;;   | send R /  `----------->|        |<-----------'  send R / |
;;;   | recv R                 | closed |               recv R   |
;;;   `----------------------->|        |<----------------------'
;;;                            +--------+

(deftype stream-state ()
  '(member :idle :reserved-local :reserved-remote :open
          :half-closed-local :half-closed-remote :closed))

(defstruct http2-stream
  "HTTP/2 stream"
  (id 0 :type (unsigned-byte 31) :read-only t)
  (state :idle :type stream-state)
  (send-window (make-flow-control-window))
  (recv-window (make-flow-control-window))
  (lock (bt:make-lock "stream-lock"))
  (priority-weight 16 :type (unsigned-byte 8))
  (priority-depends-on nil :type (or null (unsigned-byte 31)))
  (priority-exclusive nil :type boolean)
  (headers-received nil :type list)  ; List of (name . value) headers
  (data-received nil :type list)     ; List of byte arrays
  (trailers-received nil :type list) ; List of (name . value) trailers
  (end-stream-received nil :type boolean)
  (end-stream-sent nil :type boolean))

;;; Convenience accessor aliases (shorter names for common operations)

(defun stream-id (stream)
  "Get stream ID"
  (http2-stream-id stream))

(defun stream-state (stream)
  "Get stream state"
  (http2-stream-state stream))

(defun (setf stream-state) (new-state stream)
  "Set stream state"
  (setf (http2-stream-state stream) new-state))

(defun stream-send-window (stream)
  "Get stream send window"
  (http2-stream-send-window stream))

(defun stream-recv-window (stream)
  "Get stream receive window"
  (http2-stream-recv-window stream))

;;; Stream State Transitions

(defun stream-send-headers (stream)
  "Transition state on sending HEADERS frame"
  (bt:with-lock-held ((http2-stream-lock stream))
    (ecase (http2-stream-state stream)
      (:idle
       (setf (http2-stream-state stream) :open))
      (:reserved-local
       (setf (http2-stream-state stream) :half-closed-remote))
      ((:open :half-closed-remote)
       ;; Can send headers (trailers) in these states
       t)
      ((:reserved-remote :half-closed-local :closed)
       (signal-protocol-error "Cannot send HEADERS in state ~A"
                            :stream-id (http2-stream-id stream))))))

(defun stream-recv-headers (stream)
  "Transition state on receiving HEADERS frame"
  (bt:with-lock-held ((http2-stream-lock stream))
    (ecase (http2-stream-state stream)
      (:idle
       (setf (http2-stream-state stream) :open))
      (:reserved-remote
       (setf (http2-stream-state stream) :half-closed-local))
      ((:open :half-closed-local)
       ;; Can receive headers (trailers) in these states
       t)
      ((:reserved-local :half-closed-remote :closed)
       (signal-protocol-error "Cannot receive HEADERS in state ~A"
                            :stream-id (http2-stream-id stream))))))

(defun stream-send-end-stream (stream)
  "Transition state on sending END_STREAM flag"
  (bt:with-lock-held ((http2-stream-lock stream))
    (setf (http2-stream-end-stream-sent stream) t)
    (ecase (http2-stream-state stream)
      (:open
       (setf (http2-stream-state stream) :half-closed-local))
      (:half-closed-remote
       (setf (http2-stream-state stream) :closed))
      ((:idle :reserved-local :reserved-remote :half-closed-local :closed)
       (signal-protocol-error "Cannot send END_STREAM in state ~A"
                            :stream-id (http2-stream-id stream))))))

(defun stream-recv-end-stream (stream)
  "Transition state on receiving END_STREAM flag"
  (bt:with-lock-held ((http2-stream-lock stream))
    (setf (http2-stream-end-stream-received stream) t)
    (ecase (http2-stream-state stream)
      (:open
       (setf (http2-stream-state stream) :half-closed-remote))
      (:half-closed-local
       (setf (http2-stream-state stream) :closed))
      ((:idle :reserved-local :reserved-remote :half-closed-remote :closed)
       (signal-protocol-error "Cannot receive END_STREAM in state ~A"
                            :stream-id (http2-stream-id stream))))))

(defun stream-send-rst-stream (stream)
  "Transition state on sending RST_STREAM"
  (bt:with-lock-held ((http2-stream-lock stream))
    (unless (eq (http2-stream-state stream) :idle)
      (setf (http2-stream-state stream) :closed))))

(defun stream-recv-rst-stream (stream)
  "Transition state on receiving RST_STREAM"
  (bt:with-lock-held ((http2-stream-lock stream))
    (unless (eq (http2-stream-state stream) :idle)
      (setf (http2-stream-state stream) :closed))))

(defun stream-send-push-promise (stream)
  "Transition on sending PUSH_PROMISE (creates reserved-local stream)"
  (bt:with-lock-held ((http2-stream-lock stream))
    (ecase (http2-stream-state stream)
      (:idle
       (setf (http2-stream-state stream) :reserved-local))
      ((:reserved-local :reserved-remote :open :half-closed-local
        :half-closed-remote :closed)
       (signal-protocol-error "Cannot send PUSH_PROMISE in state ~A"
                            :stream-id (http2-stream-id stream))))))

(defun stream-recv-push-promise (stream)
  "Transition on receiving PUSH_PROMISE (creates reserved-remote stream)"
  (bt:with-lock-held ((http2-stream-lock stream))
    (ecase (http2-stream-state stream)
      (:idle
       (setf (http2-stream-state stream) :reserved-remote))
      ((:reserved-local :reserved-remote :open :half-closed-local
        :half-closed-remote :closed)
       (signal-protocol-error "Cannot receive PUSH_PROMISE in state ~A"
                            :stream-id (http2-stream-id stream))))))

;;; Stream Validation

(defun stream-can-send-data-p (stream)
  "Check if stream can send DATA frames"
  (member (http2-stream-state stream)
          '(:open :half-closed-remote)))

(defun stream-can-recv-data-p (stream)
  "Check if stream can receive DATA frames"
  (member (http2-stream-state stream)
          '(:open :half-closed-local)))

(defun stream-is-closed-p (stream)
  "Check if stream is closed"
  (eq (http2-stream-state stream) :closed))

(defun stream-is-idle-p (stream)
  "Check if stream is idle"
  (eq (http2-stream-state stream) :idle))

;;; Stream Data Management

(defun stream-add-received-headers (stream headers)
  "Add received headers to stream"
  (bt:with-lock-held ((http2-stream-lock stream))
    (setf (http2-stream-headers-received stream)
          (append (http2-stream-headers-received stream) headers))))

(defun stream-add-received-data (stream data)
  "Add received data to stream"
  (bt:with-lock-held ((http2-stream-lock stream))
    (push data (http2-stream-data-received stream))))

(defun stream-add-received-trailers (stream trailers)
  "Add received trailers to stream"
  (bt:with-lock-held ((http2-stream-lock stream))
    (setf (http2-stream-trailers-received stream)
          (append (http2-stream-trailers-received stream) trailers))))

(defun stream-get-all-received-data (stream)
  "Get all received data as a single byte array"
  (bt:with-lock-held ((http2-stream-lock stream))
    (let ((chunks (reverse (http2-stream-data-received stream))))
      (if (null chunks)
          (make-byte-array 0)
          (let* ((total-size (reduce #'+ chunks :key #'length))
                 (result (make-byte-array total-size))
                 (offset 0))
            (dolist (chunk chunks)
              (copy-bytes chunk result :start2 offset)
              (incf offset (length chunk)))
            result)))))

;;; Stream Priority

(defun stream-set-priority (stream weight depends-on exclusive)
  "Set stream priority information"
  (bt:with-lock-held ((http2-stream-lock stream))
    (setf (http2-stream-priority-weight stream) weight
          (http2-stream-priority-depends-on stream) depends-on
          (http2-stream-priority-exclusive stream) exclusive)))

;;; Stream Reset

(defun stream-reset (stream error-code)
  "Reset stream with given error code"
  (stream-send-rst-stream stream)
  ;; Could also clean up stream resources here
  error-code)
