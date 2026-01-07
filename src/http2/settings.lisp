;;;; settings.lisp - HTTP/2 SETTINGS frame handling (RFC 9113 Section 6.5)

(in-package #:clgrpc.http2)

;;; SETTINGS Parameters (RFC 9113 Section 6.5.2)

(defconstant +settings-header-table-size+ 1
  "Allows sender to inform remote endpoint of maximum size of header compression table")

(defconstant +settings-enable-push+ 2
  "Can be used to disable server push (0 = disabled, 1 = enabled)")

(defconstant +settings-max-concurrent-streams+ 3
  "Indicates maximum number of concurrent streams sender will allow")

(defconstant +settings-initial-window-size+ 4
  "Indicates sender's initial window size for stream-level flow control")

(defconstant +settings-max-frame-size+ 5
  "Indicates size of largest frame payload sender is willing to receive")

(defconstant +settings-max-header-list-size+ 6
  "Advises peer of maximum size of header list sender is prepared to accept")

;;; Default SETTINGS values

(defconstant +default-header-table-size+ 4096
  "Default HPACK dynamic table size (4KB)")

(defconstant +default-enable-push+ 1
  "Server push enabled by default")

(defconstant +default-initial-window-size+ 65535
  "Default initial window size (64KB - 1)")

(defconstant +default-max-frame-size+ 16384
  "Default maximum frame size (16KB)")

;;; SETTINGS Structure

(defstruct http2-settings
  "HTTP/2 SETTINGS parameters"
  (header-table-size +default-header-table-size+ :type (unsigned-byte 32))
  (enable-push +default-enable-push+ :type (unsigned-byte 32))
  (max-concurrent-streams nil :type (or null (unsigned-byte 32)))
  (initial-window-size +default-initial-window-size+ :type (unsigned-byte 32))
  (max-frame-size +default-max-frame-size+ :type (unsigned-byte 32))
  (max-header-list-size nil :type (or null (unsigned-byte 32))))

(defun make-default-settings ()
  "Create HTTP/2 settings with default values"
  (make-http2-settings))

;;; SETTINGS Frame Parsing

(defun parse-settings-frame (frame)
  "Parse SETTINGS frame payload into settings structure.
   Returns http2-settings object."
  (when (frame-flag-set-p frame +flag-ack+)
    ;; ACK frames have empty payload
    (return-from parse-settings-frame nil))

  (let ((payload (frame-payload frame))
        (settings (make-default-settings)))

    ;; Each setting is 6 bytes: 2-byte ID + 4-byte value
    (when (not (zerop (mod (length payload) 6)))
      (signal-protocol-error "SETTINGS payload length must be multiple of 6"))

    (loop for offset from 0 below (length payload) by 6
          do (let ((id (decode-uint16-be payload offset))
                  (value (decode-uint32-be payload (+ offset 2))))

               (case id
                 (#.+settings-header-table-size+
                  (setf (http2-settings-header-table-size settings) value))

                 (#.+settings-enable-push+
                  (unless (or (= value 0) (= value 1))
                    (signal-protocol-error "SETTINGS_ENABLE_PUSH must be 0 or 1"))
                  (setf (http2-settings-enable-push settings) value))

                 (#.+settings-max-concurrent-streams+
                  (setf (http2-settings-max-concurrent-streams settings) value))

                 (#.+settings-initial-window-size+
                  (when (> value #x7FFFFFFF)
                    (signal-flow-control-error "SETTINGS_INITIAL_WINDOW_SIZE too large"))
                  (setf (http2-settings-initial-window-size settings) value))

                 (#.+settings-max-frame-size+
                  (when (or (< value 16384) (> value 16777215))
                    (signal-protocol-error "SETTINGS_MAX_FRAME_SIZE out of range"))
                  (setf (http2-settings-max-frame-size settings) value))

                 (#.+settings-max-header-list-size+
                  (setf (http2-settings-max-header-list-size settings) value))

                 ;; Unknown settings are ignored per RFC
                 (t nil))))

    settings))

(defun build-settings-frame-payload (settings)
  "Build SETTINGS frame payload from settings structure.
   Returns byte array."
  (let ((params nil))

    ;; Only include non-default values
    (unless (= (http2-settings-header-table-size settings)
              +default-header-table-size+)
      (push (cons +settings-header-table-size+
                 (http2-settings-header-table-size settings))
            params))

    (unless (= (http2-settings-enable-push settings)
              +default-enable-push+)
      (push (cons +settings-enable-push+
                 (http2-settings-enable-push settings))
            params))

    (when (http2-settings-max-concurrent-streams settings)
      (push (cons +settings-max-concurrent-streams+
                 (http2-settings-max-concurrent-streams settings))
            params))

    (unless (= (http2-settings-initial-window-size settings)
              +default-initial-window-size+)
      (push (cons +settings-initial-window-size+
                 (http2-settings-initial-window-size settings))
            params))

    (unless (= (http2-settings-max-frame-size settings)
              +default-max-frame-size+)
      (push (cons +settings-max-frame-size+
                 (http2-settings-max-frame-size settings))
            params))

    (when (http2-settings-max-header-list-size settings)
      (push (cons +settings-max-header-list-size+
                 (http2-settings-max-header-list-size settings))
            params))

    ;; Build payload
    (let* ((payload-size (* 6 (length params)))
           (payload (make-byte-array payload-size)))

      (loop for (id . value) in params
            for offset from 0 by 6
            do (encode-uint16-be id payload offset)
               (encode-uint32-be value payload (+ offset 2)))

      payload)))

(defun make-settings-frame-from-settings (settings)
  "Create SETTINGS frame from settings structure"
  (let ((payload (build-settings-frame-payload settings)))
    (make-settings-frame :settings
                        (loop for offset from 0 below (length payload) by 6
                              collect (cons (decode-uint16-be payload offset)
                                          (decode-uint32-be payload (+ offset 2)))))))

(defun apply-settings (current-settings new-settings)
  "Apply new settings to current settings. Returns updated settings."
  (let ((updated (make-http2-settings
                  :header-table-size (http2-settings-header-table-size new-settings)
                  :enable-push (http2-settings-enable-push new-settings)
                  :max-concurrent-streams (http2-settings-max-concurrent-streams new-settings)
                  :initial-window-size (http2-settings-initial-window-size new-settings)
                  :max-frame-size (http2-settings-max-frame-size new-settings)
                  :max-header-list-size (http2-settings-max-header-list-size new-settings))))
    updated))
