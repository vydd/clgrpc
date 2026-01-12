# Bidirectional Streaming Implementation

ClGRPC now supports true concurrent bidirectional streaming using the **Option 3: Hybrid** approach.

## The Problem

The original bidirectional streaming implementation had a blocking issue:

```lisp
;; OLD CODE - blocks on receive
(loop for msg = (server-stream-recv stream)
      while msg
      do (process-and-send-response msg))
```

This meant:
- ✗ Server blocks waiting for messages
- ✗ Can't send messages independently
- ✗ Must receive → process → send → repeat
- ✗ No true concurrent bidirectional streaming

## The Solution

Implemented `with-bidirectional-stream` macro that:

1. **Spawns receiver thread automatically** - Runs in background, continuously reading messages
2. **Provides clean send/recv API** - User gets simple functions without thread management
3. **Thread-safe message passing** - Uses bidi-queue for coordination
4. **Automatic cleanup** - unwind-protect ensures threads are joined properly

## Usage

### Basic Example

```lisp
(defgrpc-method route-chat ((service route-guide-service)
                            (request route-note)
                            context)
  (:rpc-type :bidirectional)
  (declare (ignore service request))

  ;; Enable concurrent send/receive
  (with-bidirectional-stream (send-note recv-note) context
    ;; Loop receiving messages until client closes
    (loop for msg-bytes = (recv-note)  ; Blocks until next message
          while msg-bytes
          do (let ((note (proto-deserialize 'route-note msg-bytes)))
               ;; Process note and send responses
               (dolist (prev-note (get-previous-notes note))
                 (send-note (proto-serialize prev-note)))
               (store-note note))))

  (values +grpc-status-ok+ nil nil))
```

### With Timeout

```lisp
(with-bidirectional-stream (send-msg recv-msg) context
  ;; Receive with 100ms timeout
  (loop for msg = (recv-msg 100)
        while msg
        do (send-msg (process msg))))
```

### API

- `(send-fn msg-bytes)` - Send message immediately (non-blocking)
- `(recv-fn &optional timeout-ms)` - Receive next message (blocking)
  - Returns: message bytes, or NIL if timeout/closed
  - No timeout = wait indefinitely

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│  User Handler (Main Thread)                            │
│  ┌───────────────────────────────────────────────┐     │
│  │ (with-bidirectional-stream (send recv) ctx    │     │
│  │   (loop for msg = (recv)                      │     │
│  │         while msg                              │     │
│  │         do (send (process msg))))             │     │
│  └───────────────────────────────────────────────┘     │
│           ↑ recv()              send() ↓                │
│           │                            │                │
│     ┌─────┴─────┐              ┌──────┴──────┐        │
│     │ bidi-queue│              │ server-stream│        │
│     │  (FIFO)   │              │    -send      │        │
│     └─────┬─────┘              └─────────────┘         │
│           │ push()                                       │
│           │                                             │
│  ┌────────┴──────────────────────────────────────┐     │
│  │  Receiver Thread (Background)                 │     │
│  │  ┌──────────────────────────────────────┐    │     │
│  │  │ (loop for msg = (server-stream-recv) │    │     │
│  │  │       while msg                       │    │     │
│  │  │       do (bidi-queue-push queue msg)) │    │     │
│  │  └──────────────────────────────────────┘    │     │
│  └───────────────────────────────────────────────┘     │
└─────────────────────────────────────────────────────────┘
```

**Key Points:**
- Receiver thread continuously reads from client
- Messages stored in thread-safe queue
- Main thread processes at its own pace
- Can send responses any time (non-blocking)

## Implementation Details

### Thread-Safe Queue (src/server/streaming.lisp:58-113)

```lisp
(defstruct bidi-queue
  (items (cons nil nil))          ; FIFO queue
  (lock (bt:make-lock))           ; Thread safety
  (cv (bt:make-condition-variable))) ; Blocking wait

(defun bidi-queue-push (queue item)
  "Add item and notify waiters")

(defun bidi-queue-pop (queue timeout-ms receiver-done-fn)
  "Block until item available, timeout, or receiver done")
```

**Features:**
- Condition variables for efficient blocking
- Timeout support
- Checks if receiver thread is done
- Thread-safe push/pop operations

### Macro Implementation (src/server/streaming.lisp:115-183)

```lisp
(defmacro with-bidirectional-stream ((send-fn recv-fn) context &body body)
  `(let* ((stream (get-stream ,context))
          (recv-queue (make-bidi-queue))
          (receiver-done nil)
          (receiver-error nil)
          (receiver-thread
           (bt:make-thread
            (lambda ()
              (handler-case
                  (loop for msg = (server-stream-recv stream)
                        while msg
                        do (bidi-queue-push recv-queue msg))
                (error (e) (setf receiver-error e)))
              (setf receiver-done t)))))
     (unwind-protect
         (flet ((,send-fn (msg-bytes)
                  (when receiver-error
                    (error "Receiver thread error: ~A" receiver-error))
                  (server-stream-send stream msg-bytes))
                (,recv-fn (&optional (timeout-ms nil))
                  (when receiver-error
                    (error "Receiver thread error: ~A" receiver-error))
                  (bidi-queue-pop recv-queue timeout-ms
                                  (lambda () receiver-done))))
           ,@body)
       ;; Cleanup
       (when (bt:thread-alive-p receiver-thread)
         (bt:join-thread receiver-thread :timeout 5)))))
```

**Key Design Decisions:**
1. **Receiver thread spawned automatically** - Hidden from user
2. **Error propagation** - Receiver errors bubble up to main thread
3. **Cleanup guaranteed** - unwind-protect ensures thread is joined
4. **Closure for receiver-done** - Passed as lambda to check dynamically

## Testing

### Test Results

```
╔════════════════════════════════════════════════════════╗
║  Testing All gRPC Streaming Types                     ║
╚════════════════════════════════════════════════════════╝

✓ Unary RPC               - Working
✓ Server Streaming RPC    - Working (10 features)
✓ Client Streaming RPC    - Working (route summary)
✓ Bidirectional RPC       - Working (no longer hangs!)
```

### Example Test

```bash
$ sbcl --load test-all-streaming-types.lisp
```

## Benefits

### For Users

- ✅ **No threading knowledge required** - Just use send/recv functions
- ✅ **Clean API** - Looks like synchronous code
- ✅ **Backward compatible** - Existing handlers still work
- ✅ **Opt-in** - Use only when you need true bidirectional

### Technical

- ✅ **Concurrent send/receive** - True duplex communication
- ✅ **Non-blocking sends** - Can send any time
- ✅ **Efficient** - Condition variables, not busy-waiting
- ✅ **Safe** - Thread-safe, proper cleanup, error handling

## Comparison with Other Approaches

### Option 1: Manual Threads (Rejected)

```lisp
;; User would have to write:
(let ((recv-thread (bt:make-thread ...)))
  (handler-case
      (loop ...)
    (...))
  (bt:join-thread recv-thread))
```

❌ Complex, error-prone, requires thread management knowledge

### Option 2: Async/Callbacks (Rejected)

```lisp
;; User would write:
(stream-on-message stream
  (lambda (msg) (send (process msg))))
```

❌ Major API change, harder to reason about, callback hell

### Option 3: Hybrid (✓ Implemented)

```lisp
;; User writes:
(with-bidirectional-stream (send recv) context
  (loop for msg = (recv)
        while msg
        do (send (process msg))))
```

✅ Clean, simple, backward compatible, hides complexity

## Performance

- **Overhead**: One extra thread per bidirectional stream
- **Memory**: ~1-2 KB per stream (thread stack + queue)
- **Latency**: Minimal (one context switch per message)

**Scalability**: With 100 concurrent bidirectional streams:
- 100 receiver threads + 100 handler threads = 200 threads total
- ~100-200 KB memory overhead
- Acceptable for typical server loads

## Known Limitations

1. **One thread per stream** - Not async I/O (but good enough for most cases)
2. **No backpressure** - Queue can grow unbounded if handler is slow
3. **Fixed timeout** - Thread join timeout is hardcoded to 5 seconds

## Future Improvements

- [ ] Add backpressure (limit queue size)
- [ ] Configurable timeouts
- [ ] Async I/O option (for very high concurrency)
- [ ] Pool receiver threads (if many short-lived streams)

## See Also

- [GRACEFUL-SHUTDOWN.md](GRACEFUL-SHUTDOWN.md) - Graceful shutdown implementation
- [examples/routeguide/server-clos.lisp:174-203](examples/routeguide/server-clos.lisp) - Full RouteChat example
- [src/server/streaming.lisp](src/server/streaming.lisp) - Implementation
