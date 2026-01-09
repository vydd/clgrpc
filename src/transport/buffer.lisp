;;;; buffer.lisp - Buffered I/O for efficient HTTP/2 frame handling
;;;;
;;;; Provides buffered reading and writing over TCP or TLS sockets

(in-package #:clgrpc.transport)

;;; Buffered socket structure

(defstruct buffered-socket
  "Buffered I/O wrapper for tcp-socket or tls-socket."
  socket           ; Either tcp-socket or tls-socket
  read-buffer      ; Byte array for read buffering
  read-pos         ; Current read position in buffer
  read-limit       ; Limit of valid data in read buffer
  write-buffer     ; Byte array for write buffering
  write-pos)       ; Current write position in write buffer

;;; Buffer creation

(defun wrap-socket-with-buffer (socket &key (read-buffer-size 8192) (write-buffer-size 8192))
  "Create a buffered socket wrapping tcp-socket or tls-socket."
  (make-buffered-socket
   :socket socket
   :read-buffer (make-array read-buffer-size :element-type '(unsigned-byte 8))
   :read-pos 0
   :read-limit 0
   :write-buffer (make-array write-buffer-size :element-type '(unsigned-byte 8))
   :write-pos 0))

;;; Socket type detection and dispatch

(defun socket-type (socket)
  "Determine if socket is tcp-socket or tls-socket."
  (cond
    ((tcp-socket-p socket) :tcp)
    ((tls-socket-p socket) :tls)
    (t (error "Unknown socket type: ~A" socket))))

(defun raw-read-sequence (socket buffer &key (start 0) (end nil))
  "Read sequence from underlying socket."
  (ecase (socket-type socket)
    (:tcp (socket-read-sequence socket buffer :start start :end end))
    (:tls (tls-read-sequence socket buffer :start start :end end))))

(defun raw-write-sequence (socket sequence &key (start 0) (end nil))
  "Write sequence to underlying socket."
  (ecase (socket-type socket)
    (:tcp (socket-write-sequence socket sequence :start start :end end))
    (:tls (tls-write-sequence socket sequence :start start :end end))))

(defun raw-flush (socket)
  "Flush underlying socket."
  (ecase (socket-type socket)
    (:tcp (socket-flush socket))
    (:tls (tls-flush socket))))

(defun raw-close (socket)
  "Close underlying socket."
  (ecase (socket-type socket)
    (:tcp (socket-close socket))
    (:tls (tls-close socket))))

(defun raw-open-p (socket)
  "Check if underlying socket is open."
  (ecase (socket-type socket)
    (:tcp (socket-open-p socket))
    (:tls (tls-open-p socket))))

;;; Buffered reading

(defun fill-read-buffer (buffered-socket)
  "Fill read buffer from underlying socket. Returns number of bytes read, or 0 on EOF."
  (let* ((buffer (buffered-socket-read-buffer buffered-socket))
         (socket (buffered-socket-socket buffered-socket))
         (bytes-read (raw-read-sequence socket buffer)))
    (setf (buffered-socket-read-pos buffered-socket) 0)
    (setf (buffered-socket-read-limit buffered-socket) bytes-read)
    bytes-read))

(defun buffered-read-byte (buffered-socket)
  "Read a single byte from buffered socket. Returns byte or nil on EOF."
  (let ((pos (buffered-socket-read-pos buffered-socket))
        (limit (buffered-socket-read-limit buffered-socket)))
    ;; Refill buffer if empty
    (when (>= pos limit)
      (let ((bytes-read (fill-read-buffer buffered-socket)))
        (when (zerop bytes-read)
          (return-from buffered-read-byte nil))
        (setf pos 0
              limit bytes-read)))
    ;; Read byte from buffer
    (let ((byte (aref (buffered-socket-read-buffer buffered-socket) pos)))
      (setf (buffered-socket-read-pos buffered-socket) (1+ pos))
      byte)))

(defun buffered-read-bytes (buffered-socket count)
  "Read exactly count bytes from buffered socket. Signals error if EOF before count bytes read."
  (let ((result (make-array count :element-type '(unsigned-byte 8)))
        (result-pos 0))
    (loop while (< result-pos count)
          do (let* ((pos (buffered-socket-read-pos buffered-socket))
                    (limit (buffered-socket-read-limit buffered-socket))
                    (available (- limit pos)))
               ;; Refill if buffer empty
               (when (zerop available)
                 (let ((bytes-read (fill-read-buffer buffered-socket)))
                   (when (zerop bytes-read)
                     (error "Unexpected EOF: read ~D bytes, expected ~D" result-pos count))
                   (setf pos 0
                         limit bytes-read
                         available bytes-read)))
               ;; Copy from buffer
               (let* ((to-copy (min available (- count result-pos)))
                      (buffer (buffered-socket-read-buffer buffered-socket)))
                 (replace result buffer
                          :start1 result-pos
                          :start2 pos
                          :end2 (+ pos to-copy))
                 (incf result-pos to-copy)
                 (setf (buffered-socket-read-pos buffered-socket) (+ pos to-copy)))))
    result))

(defun buffered-peek-bytes (buffered-socket count)
  "Peek at next count bytes without consuming them. Critical for HTTP/2 frame header parsing."
  (let ((pos (buffered-socket-read-pos buffered-socket))
        (limit (buffered-socket-read-limit buffered-socket))
        (available (- (buffered-socket-read-limit buffered-socket)
                     (buffered-socket-read-pos buffered-socket))))
    ;; Refill if not enough data
    (when (< available count)
      (let ((buffer (buffered-socket-read-buffer buffered-socket)))
        ;; Move remaining data to start of buffer
        (when (> available 0)
          (replace buffer buffer :start2 pos :end2 limit))
        (setf (buffered-socket-read-pos buffered-socket) 0)
        (setf (buffered-socket-read-limit buffered-socket) available)
        ;; Read more data
        (let* ((socket (buffered-socket-socket buffered-socket))
               (bytes-read (raw-read-sequence socket buffer
                                             :start available
                                             :end (length buffer))))
          (when (zerop bytes-read)
            (error "Unexpected EOF: cannot peek ~D bytes" count))
          (setf (buffered-socket-read-limit buffered-socket) (+ available bytes-read))
          (setf pos 0
                limit (+ available bytes-read)
                available (+ available bytes-read)))))
    ;; Return copy of peeked bytes
    (let ((result (make-array count :element-type '(unsigned-byte 8)))
          (buffer (buffered-socket-read-buffer buffered-socket)))
      (replace result buffer :start2 pos :end2 (+ pos count))
      result)))

(defun buffered-skip-bytes (buffered-socket count)
  "Skip count bytes (use after peeking)."
  (incf (buffered-socket-read-pos buffered-socket) count))

;;; Buffered writing

(defun flush-write-buffer (buffered-socket)
  "Flush write buffer to underlying socket."
  (let ((pos (buffered-socket-write-pos buffered-socket)))
    (when (> pos 0)
      (let ((socket (buffered-socket-socket buffered-socket))
            (buffer (buffered-socket-write-buffer buffered-socket)))
        (raw-write-sequence socket buffer :start 0 :end pos)
        (raw-flush socket)
        (setf (buffered-socket-write-pos buffered-socket) 0)))))

(defun buffered-write-byte (buffered-socket byte)
  "Write a single byte to buffered socket."
  (let ((pos (buffered-socket-write-pos buffered-socket))
        (buffer (buffered-socket-write-buffer buffered-socket)))
    ;; Flush if buffer full
    (when (>= pos (length buffer))
      (flush-write-buffer buffered-socket)
      (setf pos 0))
    ;; Write byte to buffer
    (setf (aref buffer pos) byte)
    (setf (buffered-socket-write-pos buffered-socket) (1+ pos))))

(defun buffered-write-bytes (buffered-socket bytes)
  "Write byte array to buffered socket."
  (let ((bytes-len (length bytes))
        (buffer-size (length (buffered-socket-write-buffer buffered-socket))))
    (cond
      ;; Large write - bypass buffer
      ((> bytes-len buffer-size)
       (flush-write-buffer buffered-socket)
       (raw-write-sequence (buffered-socket-socket buffered-socket) bytes))
      ;; Normal write - use buffer
      (t
       (let ((pos (buffered-socket-write-pos buffered-socket))
             (buffer (buffered-socket-write-buffer buffered-socket)))
         ;; Flush if won't fit
         (when (> (+ pos bytes-len) buffer-size)
           (flush-write-buffer buffered-socket)
           (setf pos 0))
         ;; Copy to buffer
         (replace buffer bytes :start1 pos)
         (setf (buffered-socket-write-pos buffered-socket) (+ pos bytes-len)))))))

(defun buffered-flush (buffered-socket)
  "Explicitly flush write buffer."
  (flush-write-buffer buffered-socket))

;;; Control operations

(defun buffered-close (buffered-socket)
  "Close buffered socket (flushes write buffer first)."
  (flush-write-buffer buffered-socket)
  (raw-close (buffered-socket-socket buffered-socket)))

(defun buffered-open-p (buffered-socket)
  "Check if buffered socket is open."
  (raw-open-p (buffered-socket-socket buffered-socket)))

(defun buffered-get-underlying-socket (buffered-socket)
  "Get the underlying tcp-socket or tls-socket."
  (buffered-socket-socket buffered-socket))

;;; Utilities

(defun buffered-read-available (buffered-socket)
  "Return number of bytes available in read buffer without reading from socket."
  (- (buffered-socket-read-limit buffered-socket)
     (buffered-socket-read-pos buffered-socket)))

(defun buffered-write-available (buffered-socket)
  "Return available space in write buffer."
  (- (length (buffered-socket-write-buffer buffered-socket))
     (buffered-socket-write-pos buffered-socket)))

(defmacro with-buffered-socket ((socket-var socket &rest options) &body body)
  "Execute body with a buffered socket, ensuring cleanup."
  `(let ((,socket-var (wrap-socket-with-buffer ,socket ,@options)))
     (unwind-protect
          (progn ,@body)
       (when ,socket-var
         (buffered-close ,socket-var)))))
