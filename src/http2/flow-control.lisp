;;;; flow-control.lisp - HTTP/2 flow control (RFC 9113 Section 5.2)

(in-package #:clgrpc.http2)

;;; Flow Control Window

(defstruct flow-control-window
  "Flow control window for HTTP/2 stream or connection"
  (size +default-initial-window-size+ :type (signed-byte 32))
  (lock (bt:make-lock "flow-control-lock")))

(defun window-available-p (window)
  "Check if window has space available"
  (bt:with-lock-held ((flow-control-window-lock window))
    (plusp (flow-control-window-size window))))

(defun window-consume (window amount)
  "Consume amount from window. Signals error if insufficient space.
   Returns new window size."
  (bt:with-lock-held ((flow-control-window-lock window))
    (when (> amount (flow-control-window-size window))
      (signal-flow-control-error "Flow control window exceeded"))
    (decf (flow-control-window-size window) amount)
    (flow-control-window-size window)))

(defun window-replenish (window amount)
  "Replenish window by amount (from WINDOW_UPDATE).
   Returns new window size."
  (bt:with-lock-held ((flow-control-window-lock window))
    (let ((new-size (+ (flow-control-window-size window) amount)))
      ;; Check for overflow
      (when (> new-size #x7FFFFFFF)
        (signal-flow-control-error "Flow control window overflow"))
      (setf (flow-control-window-size window) new-size)
      new-size)))

(defun window-size (window)
  "Get current window size (thread-safe)"
  (bt:with-lock-held ((flow-control-window-lock window))
    (flow-control-window-size window)))

(defun window-set-size (window new-size)
  "Set window size (used for SETTINGS initial window size updates)"
  (bt:with-lock-held ((flow-control-window-lock window))
    (when (> new-size #x7FFFFFFF)
      (signal-flow-control-error "Window size too large"))
    (setf (flow-control-window-size window) new-size)))

;;; Flow Control for Sending Data

(defun compute-send-amount (stream-window connection-window max-frame-size data-length)
  "Compute amount of data that can be sent given flow control constraints.
   Returns amount to send."
  (min data-length
       max-frame-size
       (max 0 stream-window)
       (max 0 connection-window)))

(defun flow-control-send-data (stream-window connection-window data &key max-frame-size)
  "Send data respecting flow control. Returns list of data chunks to send.
   Each chunk is within flow control limits and max frame size."
  (let ((max-size (or max-frame-size +default-max-frame-size+))
        (chunks nil)
        (offset 0))

    (loop while (< offset (length data))
          do (let* ((remaining (- (length data) offset))
                   (stream-avail (window-size stream-window))
                   (conn-avail (window-size connection-window))
                   (amount (compute-send-amount stream-avail conn-avail
                                               max-size remaining)))

               (when (zerop amount)
                 ;; No window space - must wait
                 (return))

               ;; Consume window
               (window-consume stream-window amount)
               (window-consume connection-window amount)

               ;; Extract chunk
               (push (subseq data offset (+ offset amount)) chunks)
               (incf offset amount)))

    (values (nreverse chunks) offset)))

;;; Processing WINDOW_UPDATE frames

(defun process-window-update (window frame)
  "Process WINDOW_UPDATE frame and update window.
   Returns new window size."
  (let ((payload (frame-payload frame)))
    (when (< (length payload) 4)
      (signal-protocol-error "WINDOW_UPDATE payload too small"))

    (let ((increment (logand (decode-uint32-be payload 0) #x7FFFFFFF)))
      (when (zerop increment)
        (signal-protocol-error "WINDOW_UPDATE increment must be non-zero"))

      (window-replenish window increment))))

;;; Flow Control State

(defstruct flow-control-state
  "Flow control state for a connection"
  (connection-send-window (make-flow-control-window))
  (connection-recv-window (make-flow-control-window))
  (update-initial-window-size +default-initial-window-size+
                             :type (unsigned-byte 32)))

(defun update-all-stream-windows (streams old-size new-size)
  "Update all stream windows when SETTINGS changes initial window size"
  (let ((delta (- new-size old-size)))
    (loop for stream being the hash-values of streams
          do (let ((window (stream-send-window stream)))
               (when window
                 (window-replenish window delta))))))
