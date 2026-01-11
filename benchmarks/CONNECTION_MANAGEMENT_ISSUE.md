# Connection Management Performance Issue

## Problem Summary

The current benchmark creates **one HTTP/2 connection per worker thread**, resulting in 100 separate connections for the 100-client benchmark. This wastes HTTP/2's stream multiplexing capability and creates significant overhead.

## Current Architecture (Inefficient)

### How It Works Now

Each benchmark worker:
1. Creates its own `grpc-channel`
2. Channel creates its own `connection-pool`
3. Pool creates a new HTTP/2 connection on first request
4. Connection stays isolated to that worker

**Result:** 100 workers = 100 channels = 100 pools = **100 separate HTTP/2 connections**

```
Worker 1  → Channel 1  → Pool 1  → Connection 1  ═╗
Worker 2  → Channel 2  → Pool 2  → Connection 2  ═╣
Worker 3  → Channel 3  → Pool 3  → Connection 3  ═╬→ Server
...                                                 ║
Worker 100 → Channel 100 → Pool 100 → Connection 100 ╝

Total: 100 TCP connections, 100 frame reader threads
```

### Cost Per Connection

Creating an HTTP/2 connection involves:

1. **TCP handshake** - 3-way handshake, ~1 RTT
2. **HTTP/2 preface** - Send 24-byte magic string
3. **SETTINGS exchange** - Client → Server → ACK
4. **Thread creation** - Spawn frame reader thread
5. **Memory allocation** - Buffers, hash tables, HPACK contexts
6. **HPACK initialization** - Static + dynamic tables

**Estimated overhead:**
- ~1-2ms latency per connection (handshake + setup)
- 3-5 syscalls (connect, write×2, read×2)
- 1 thread creation (expensive on some systems)
- ~100KB memory per connection (buffers + structures)

**Total for 100 connections:**
- 100-200ms startup latency
- 300-500 syscalls
- 100 threads
- ~10MB memory overhead

## HTTP/2 Multiplexing (What We Should Use)

HTTP/2's primary feature is **stream multiplexing**: one TCP connection can handle many concurrent requests via independent streams.

### How HTTP/2 Multiplexing Works

```
Single HTTP/2 Connection:
┌─────────────────────────────────────┐
│ TCP Connection                      │
│ ┌─────────────────────────────────┐ │
│ │ Stream 1 (Worker 1's RPC)       │ │
│ ├─────────────────────────────────┤ │
│ │ Stream 3 (Worker 2's RPC)       │ │
│ ├─────────────────────────────────┤ │
│ │ Stream 5 (Worker 3's RPC)       │ │
│ ├─────────────────────────────────┤ │
│ │ ...                             │ │
│ ├─────────────────────────────────┤ │
│ │ Stream 199 (Worker 100's RPC)   │ │
│ └─────────────────────────────────┘ │
└─────────────────────────────────────┘
```

**Benefits:**
- Clients use odd stream IDs (1, 3, 5, 7, ...)
- Server uses even stream IDs (2, 4, 6, 8, ...)
- Each stream is independent (own flow control, headers, data)
- Default `SETTINGS_MAX_CONCURRENT_STREAMS` = unlimited (implementation: usually 100-250)

## Optimal Architecture (HTTP/2 Multiplexing)

### Shared Channel Approach

All workers share a **single channel** with a **shared connection pool**:

```
Worker 1  ╗
Worker 2  ║
Worker 3  ╣→ SHARED Channel → SHARED Pool → Connection 1 (handles streams 1, 3, 5, 7...)    ═╗
Worker 4  ║                               → Connection 2 (handles streams 9, 11, 13, 15...) ═╬→ Server  
...       ║                               → Connection 3 (handles streams 17, 19, 21, 23...)═╝
Worker 100╝

Total: 3 TCP connections, 3 frame reader threads, 100 concurrent streams
```

**Why 3 connections instead of 1?**
- Load balancing across connections
- Avoid head-of-line blocking at TCP level
- Parallel frame processing
- Stay under `SETTINGS_MAX_CONCURRENT_STREAMS` per connection

### Expected Performance Improvement

**From connection reuse alone: 2-5x faster**

Why:
1. **Eliminate startup overhead**
   - 100 → 3 connections = 97 fewer handshakes
   - Save 97-194ms startup latency
   - Save ~300 thread creations (over multiple test runs)

2. **Better resource utilization**
   - 3 threads vs 100 threads
   - Less context switching
   - Better CPU cache locality

3. **Less memory allocation**
   - 300KB vs 10MB for connections
   - Reduced GC pressure

4. **True HTTP/2 benefits**
   - Single HPACK context per connection (better compression)
   - Shared flow control windows
   - Frame batching opportunities

## Code Changes Required

### Current Benchmark Code

```lisp
;; PROBLEM: Each worker creates its own channel
(defun benchmark-unary-worker (duration)
  (let ((channel (make-channel "localhost:50054" :secure nil))
        ...)
    (loop ... (call-unary channel ...))
    (close-channel channel)))  ; Closes connection
```

### Fixed Benchmark Code

```lisp
;; SOLUTION: Share one channel across all workers
(defparameter *shared-channel* nil)

(defun setup-benchmark ()
  "Create shared channel before spawning workers"
  (setf *shared-channel* (make-channel "localhost:50054" :secure nil)))

(defun cleanup-benchmark ()
  "Close shared channel after all workers done"
  (when *shared-channel*
    (close-channel *shared-channel*)
    (setf *shared-channel* nil)))

(defun benchmark-unary-worker (duration)
  "Use shared channel - NO per-worker overhead"
  (let ((request-count 0)
        (error-count 0)
        ...)
    (loop ... (call-unary *shared-channel* ...))
    (values request-count error-count)))

(defun run-benchmark (...)
  (setup-benchmark)
  (unwind-protect
      (progn
        ;; Spawn workers - all use *shared-channel*
        (dotimes (i num-clients)
          (push (bt:make-thread #'benchmark-unary-worker ...) threads))
        ...)
    (cleanup-benchmark)))
```

## Thread Safety

Our connection pool is **already thread-safe** for this use case:

1. **Connection retrieval** - Protected by `connection-pool-lock`
   ```lisp
   (bordeaux-threads:with-lock-held ((connection-pool-lock pool))
     ;; Atomic: find existing OR create new connection
     ...)
   ```

2. **Stream ID allocation** - Atomic increment
   ```lisp
   (incf (http2-connection-next-stream-id conn) 2)  ; Odd IDs for client
   ```

3. **Active calls tracking** - Per-connection hash table with lock
   ```lisp
   (setf (gethash stream-id (http2-connection-active-calls conn)) call)
   ```

4. **Frame writing** - Protected by `connection-write-lock`
   ```lisp
   (bt:with-lock-held ((http2-connection-write-lock conn))
     (write-frame-to-stream frame socket))
   ```

5. **Frame reading** - Single dedicated thread per connection
   - No races: only one thread reads from socket
   - Dispatches to calls via hash table lookup

**Conclusion:** No code changes needed for thread safety - just change benchmark to share channels!

## Comparison with Go

### Go's Approach

Go's gRPC implementation:
- Uses connection pooling by default
- Maintains 1-10 connections per target
- Automatically multiplexes all RPCs over available connections
- Round-robin load balancing across connections

### Our Current Approach

- Creates separate connection per worker (not pooled across workers)
- Each connection underutilized (1 RPC at a time in benchmark)
- Missing the core HTTP/2 multiplexing benefit

### After Fix

- Matches Go's architecture
- Proper connection pooling
- Full HTTP/2 multiplexing
- Should narrow the performance gap significantly

## Next Steps

1. **Implement shared channel benchmark** - Test the theory
2. **Measure improvement** - Expect 2-5x speedup  
3. **Update documentation** - Document best practices
4. **Add example** - Show how to properly use channels in production

## References

- [HTTP/2 RFC 9113 Section 5](https://www.rfc-editor.org/rfc/rfc9113.html#section-5) - Streams and Multiplexing
- [gRPC Performance Best Practices](https://grpc.io/docs/guides/performance/) - Connection reuse
- Go gRPC implementation: `google.golang.org/grpc/clientconn.go` - Connection pool logic
