;;;; connection-tests.lisp - Tests for HTTP/2 connection management

(in-package #:clgrpc-tests)

(def-suite connection-tests
  :in http2-tests
  :description "HTTP/2 connection management tests")

(in-suite connection-tests)

;;; Connection Creation

(test connection-create-client
  "Test creating client connection"
  (let ((conn (make-client-connection nil)))
    (is (http2-connection-is-client conn))
    (is (= (http2-connection-next-stream-id conn) 1))))  ; Odd for client

(test connection-create-server
  "Test creating server connection"
  (let ((conn (make-server-connection nil)))
    (is (not (http2-connection-is-client conn)))
    (is (= (http2-connection-next-stream-id conn) 2))))  ; Even for server

;;; Stream Management

(test connection-create-stream
  "Test creating stream on connection"
  (let ((conn (make-client-connection nil)))
    (let ((stream (connection-create-stream conn 1)))
      (is (= (stream-id stream) 1))
      (is (not (null (connection-get-stream conn 1)))))))

(test connection-get-or-create-stream
  "Test get-or-create-stream function"
  (let ((conn (make-client-connection nil)))
    (let ((stream1 (connection-get-or-create-stream conn 1))
          (stream2 (connection-get-or-create-stream conn 1)))
      (is (eq stream1 stream2)))))  ; Same stream object

(test connection-allocate-stream-id
  "Test stream ID allocation"
  (let ((conn (make-client-connection nil)))
    (let ((id1 (connection-allocate-stream-id conn))
          (id2 (connection-allocate-stream-id conn)))
      (is (= id1 1))
      (is (= id2 3))  ; Skips by 2
      (is (oddp id1))
      (is (oddp id2)))))

;;; Settings

(test connection-default-settings
  "Test connection has default settings"
  (let ((conn (make-client-connection nil)))
    (is (not (null (http2-connection-local-settings conn))))
    (is (not (null (http2-connection-remote-settings conn))))))

(test parse-settings-frame-basic
  "Test parsing basic SETTINGS frame"
  (let* ((frame (make-settings-frame :settings '((1 . 8192) (4 . 32768))))
         (settings (parse-settings-frame frame)))
    (is (= (http2-settings-header-table-size settings) 8192))
    (is (= (http2-settings-initial-window-size settings) 32768))))

(test parse-settings-frame-ack
  "Test parsing SETTINGS ACK frame"
  (let* ((frame (make-settings-frame :ack t))
         (settings (parse-settings-frame frame)))
    (is (null settings))))  ; ACK has no payload

;;; Flow Control

(test connection-flow-control-windows
  "Test connection has flow control windows"
  (let ((conn (make-client-connection nil)))
    (is (not (null (http2-connection-connection-send-window conn))))
    (is (not (null (http2-connection-connection-recv-window conn))))))

(test flow-control-window-consume
  "Test consuming from flow control window"
  (let ((window (make-flow-control-window)))
    (let ((new-size (window-consume window 1000)))
      (is (= new-size (- +default-initial-window-size+ 1000))))))

(test flow-control-window-replenish
  "Test replenishing flow control window"
  (let ((window (make-flow-control-window)))
    (window-consume window 1000)
    (let ((new-size (window-replenish window 500)))
      (is (= new-size (- +default-initial-window-size+ 500))))))

(test flow-control-window-overflow-error
  "Test window overflow is caught"
  (let ((window (make-flow-control-window)))
    (signals http2-flow-control-error
      (window-replenish window #x7FFFFFFF))))  ; Would overflow

;;; Connection State

(test connection-initially-open
  "Test connection starts in open state"
  (let ((conn (make-client-connection nil)))
    (is (not (connection-is-closed-p conn)))))

(test connection-close
  "Test closing connection"
  (let ((conn (make-client-connection nil)))
    (connection-close conn)
    (is (connection-is-closed-p conn))))

(test connection-goaway-state
  "Test GOAWAY affects connection state"
  (let ((conn (make-client-connection nil)))
    (setf (http2-connection-goaway-sent conn) t)
    (setf (http2-connection-goaway-received conn) t)
    (is (connection-is-closed-p conn))))

;;; HPACK Contexts

(test connection-has-hpack-contexts
  "Test connection has HPACK encoder and decoder"
  (let ((conn (make-client-connection nil)))
    (is (not (null (http2-connection-hpack-encoder conn))))
    (is (not (null (http2-connection-hpack-decoder conn))))))
