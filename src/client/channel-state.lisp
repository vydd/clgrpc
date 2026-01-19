;;;; channel-state.lisp - Channel state management for gRPC clients
;;;
;;; Implements gRPC connectivity semantics with state transitions:
;;;   IDLE -> CONNECTING -> READY -> TRANSIENT_FAILURE -> SHUTDOWN

(in-package #:clgrpc.client)

;;; Channel Structure (forward declaration for connection-pool)

(defstruct grpc-channel
  "gRPC channel (connection to server)"
  (target "" :type string)
  (pool nil)  ; connection-pool, set after creation
  (active-calls (make-hash-table) :type hash-table)  ; stream-id -> grpc-call
  (calls-lock (bordeaux-threads:make-lock "channel-calls-lock"))
  (closed nil :type boolean)
  ;; Channel state management (gRPC connectivity semantics)
  (state :idle :type keyword)  ; :idle, :connecting, :ready, :transient-failure, :shutdown
  (state-lock (bordeaux-threads:make-lock "channel-state-lock"))
  (state-cv (bordeaux-threads:make-condition-variable :name "channel-state-cv")))

;;; Channel State Management

(defun channel-state (channel)
  "Get current channel state.

   Returns one of:
     :idle - No active connection, waiting for RPC
     :connecting - Attempting to establish connection
     :ready - Connected and ready to send RPCs
     :transient-failure - Connection failed, may retry
     :shutdown - Channel permanently closed"
  (bordeaux-threads:with-lock-held ((grpc-channel-state-lock channel))
    (grpc-channel-state channel)))

(defun channel-set-state (channel new-state)
  "Internal: atomically update channel state and notify waiters.

   Args:
     channel: grpc-channel
     new-state: New state keyword"
  (bordeaux-threads:with-lock-held ((grpc-channel-state-lock channel))
    (let ((old-state (grpc-channel-state channel)))
      (unless (eq old-state new-state)
        (debug-log "Channel state: ~A -> ~A~%" old-state new-state)
        (setf (grpc-channel-state channel) new-state)
        (bordeaux-threads:condition-notify (grpc-channel-state-cv channel))))))

(defun wait-for-state-change (channel from-state &key (timeout 30))
  "Block until channel leaves from-state.

   Args:
     channel: grpc-channel
     from-state: State to wait to leave
     timeout: Maximum seconds to wait (default 30)

   Returns:
     New state keyword, or nil if timeout"
  (bordeaux-threads:with-lock-held ((grpc-channel-state-lock channel))
    (loop while (eq (grpc-channel-state channel) from-state)
          do (unless (bordeaux-threads:condition-wait
                      (grpc-channel-state-cv channel)
                      (grpc-channel-state-lock channel)
                      :timeout timeout)
               (return-from wait-for-state-change nil)))
    (grpc-channel-state channel)))

(defun wait-for-ready (channel &key (timeout 30))
  "Wait until channel is READY for sending RPCs.

   Args:
     channel: grpc-channel
     timeout: Maximum seconds to wait (default 30)

   Returns:
     t if channel became ready, nil if timeout or shutdown"
  (let ((start (get-internal-real-time)))
    (loop
      (let ((state (channel-state channel)))
        (case state
          (:ready (return t))
          (:shutdown (return nil))
          (otherwise
           (let* ((elapsed (/ (- (get-internal-real-time) start)
                              internal-time-units-per-second))
                  (remaining (- timeout elapsed)))
             (when (<= remaining 0)
               (return nil))
             (wait-for-state-change channel state :timeout remaining))))))))
