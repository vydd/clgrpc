;;;; stream-tests.lisp - Tests for HTTP/2 stream state machine

(in-package #:clgrpc-tests)

(def-suite stream-tests
  :in http2-tests
  :description "HTTP/2 stream state machine tests")

(in-suite stream-tests)

;;; Stream State Transitions

(test stream-initial-state
  "Test stream starts in idle state"
  (let ((stream (make-http2-stream :id 1)))
    (is (eq (stream-state stream) :idle))))

(test stream-send-headers-from-idle
  "Test sending HEADERS transitions from idle to open"
  (let ((stream (make-http2-stream :id 1)))
    (stream-send-headers stream)
    (is (eq (stream-state stream) :open))))

(test stream-recv-headers-from-idle
  "Test receiving HEADERS transitions from idle to open"
  (let ((stream (make-http2-stream :id 1)))
    (stream-recv-headers stream)
    (is (eq (stream-state stream) :open))))

(test stream-send-end-stream-from-open
  "Test sending END_STREAM from open transitions to half-closed-local"
  (let ((stream (make-http2-stream :id 1)))
    (stream-send-headers stream)  ; idle -> open
    (stream-send-end-stream stream)  ; open -> half-closed-local
    (is (eq (stream-state stream) :half-closed-local))))

(test stream-recv-end-stream-from-open
  "Test receiving END_STREAM from open transitions to half-closed-remote"
  (let ((stream (make-http2-stream :id 1)))
    (stream-recv-headers stream)  ; idle -> open
    (stream-recv-end-stream stream)  ; open -> half-closed-remote
    (is (eq (stream-state stream) :half-closed-remote))))

(test stream-bidirectional-close
  "Test bidirectional close (both ends send END_STREAM)"
  (let ((stream (make-http2-stream :id 1)))
    (stream-send-headers stream)  ; idle -> open
    (stream-send-end-stream stream)  ; open -> half-closed-local
    (stream-recv-end-stream stream)  ; half-closed-local -> closed
    (is (eq (stream-state stream) :closed))))

(test stream-rst-stream-closes
  "Test RST_STREAM transitions to closed"
  (let ((stream (make-http2-stream :id 1)))
    (stream-send-headers stream)  ; idle -> open
    (stream-send-rst-stream stream)  ; -> closed
    (is (eq (stream-state stream) :closed))))

;;; Stream Data Management

(test stream-add-received-data
  "Test adding received data to stream"
  (let ((stream (make-http2-stream :id 1)))
    (stream-add-received-data stream (bytes 1 2 3))
    (stream-add-received-data stream (bytes 4 5 6))
    (let ((all-data (stream-get-all-received-data stream)))
      (is (equalp all-data (bytes 1 2 3 4 5 6))))))

(test stream-add-received-headers
  "Test adding received headers to stream"
  (let ((stream (make-http2-stream :id 1)))
    (stream-add-received-headers stream '((:method . "GET")))
    (stream-add-received-headers stream '((:path . "/")))
    (is (= (length (http2-stream-headers-received stream)) 2))))

;;; Flow Control Windows

(test stream-has-flow-control-windows
  "Test stream has send and receive windows"
  (let ((stream (make-http2-stream :id 1)))
    (is (not (null (stream-send-window stream))))
    (is (not (null (stream-recv-window stream))))))

(test stream-windows-initialized
  "Test stream windows are initialized to default size"
  (let ((stream (make-http2-stream :id 1)))
    (is (= (window-size (stream-send-window stream))
          +default-initial-window-size+))
    (is (= (window-size (stream-recv-window stream))
          +default-initial-window-size+))))

;;; Stream Validation

(test stream-can-send-data
  "Test stream-can-send-data-p predicate"
  (let ((stream (make-http2-stream :id 1)))
    (is (not (stream-can-send-data-p stream)))  ; idle
    (stream-send-headers stream)
    (is (stream-can-send-data-p stream))  ; open
    (stream-send-end-stream stream)
    (is (not (stream-can-send-data-p stream)))))  ; half-closed-local

(test stream-can-recv-data
  "Test stream-can-recv-data-p predicate"
  (let ((stream (make-http2-stream :id 1)))
    (is (not (stream-can-recv-data-p stream)))  ; idle
    (stream-recv-headers stream)
    (is (stream-can-recv-data-p stream))  ; open
    (stream-recv-end-stream stream)
    (is (not (stream-can-recv-data-p stream)))))  ; half-closed-remote

(test stream-is-closed
  "Test stream-is-closed-p predicate"
  (let ((stream (make-http2-stream :id 1)))
    (is (not (stream-is-closed-p stream)))
    (stream-send-headers stream)
    (is (not (stream-is-closed-p stream)))
    (stream-send-rst-stream stream)
    (is (stream-is-closed-p stream))))

;;; Priority

(test stream-set-priority
  "Test setting stream priority"
  (let ((stream (make-http2-stream :id 3)))
    (stream-set-priority stream 100 1 t)
    (is (= (http2-stream-priority-weight stream) 100))
    (is (= (http2-stream-priority-depends-on stream) 1))
    (is (http2-stream-priority-exclusive stream))))
