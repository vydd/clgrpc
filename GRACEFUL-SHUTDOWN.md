# Graceful Shutdown

ClGRPC implements proper graceful shutdown to ensure zero request failures during server restarts and deployments.

## Overview

When you stop a gRPC server, you want to:
- ✓ Complete all active requests
- ✓ Avoid dropping in-flight requests
- ✓ Cleanly close all connections
- ✓ Allow zero-downtime deployments

Without graceful shutdown, stopping a server would immediately close connections and drop active requests.

## How It Works

### Server Shutdown Sequence

```lisp
(stop-server server :timeout 30)
```

1. **Stop Accepting New Connections** (immediate)
   - Closes the listener socket
   - Rejects any new incoming connections
   - Existing connections remain active

2. **Send GOAWAY Frames** (immediate)
   - Sends HTTP/2 GOAWAY to all connected clients
   - Tells clients: "no new streams, but existing streams can finish"
   - Clients automatically use fresh connections for new requests

3. **Wait for Active Streams** (up to timeout)
   - Monitors all active requests across all connections
   - Prints progress every 5 seconds
   - Completes when all requests finish OR timeout expires

4. **Force Close** (after timeout)
   - Closes all remaining connections
   - Any stuck requests are terminated
   - Ensures server doesn't hang indefinitely

### Client Behavior

When a client receives GOAWAY:
```lisp
;; Connection marked as "received GOAWAY"
(setf (http2-connection-goaway-received conn) t)

;; Connection pool automatically:
;; 1. Completes existing streams on this connection
;; 2. Creates NEW connection for new requests
;; 3. Eventually closes old connection when streams done
```

This is transparent to application code - it Just Works™

## Usage Examples

### Basic Usage

```lisp
(defparameter *server* (make-server :port 50051))
(register-service (grpc-server-router *server*) my-service)
(start-server *server*)

;; Later: graceful shutdown with 30 second timeout
(stop-server *server* :timeout 30)
```

### Production Deployment

```lisp
(defun main ()
  (let ((server (make-server :port 50051)))
    ;; Register services
    (register-service (grpc-server-router server) my-service)

    ;; Start server
    (start-server server)

    ;; Handle SIGTERM (Kubernetes sends this)
    (handler-case
        (loop (sleep 1))  ; Keep running
      (sb-sys:interactive-interrupt ()
        ;; Ctrl+C or SIGTERM received
        (format t "Shutting down...~%")
        (stop-server server :timeout 60)
        (sb-ext:exit :code 0)))))
```

### Kubernetes Integration

```yaml
apiVersion: v1
kind: Pod
spec:
  containers:
  - name: grpc-server
    image: my-grpc-server
    lifecycle:
      preStop:
        exec:
          # Kubernetes waits for this to complete before SIGTERM
          command: ["/bin/sh", "-c", "sleep 5"]
    terminationGracePeriodSeconds: 60  # Must be > stop-server timeout
```

**What happens:**
1. Kubernetes decides to stop pod
2. Removes pod from service endpoints (no new traffic)
3. Waits 5 seconds (preStop hook)
4. Sends SIGTERM to process
5. Server initiates graceful shutdown (60s timeout)
6. Waits up to 60 seconds for server to exit
7. Sends SIGKILL if still running after 60s

## Shutdown Output

### Successful Shutdown

```
Initiating graceful shutdown (timeout: 30 seconds)...
✓ Stopped accepting new connections
✓ Sending GOAWAY to 3 connection(s)...
  Waiting for 12 active stream(s)... (0/30 seconds)
  Waiting for 8 active stream(s)... (5/30 seconds)
  Waiting for 3 active stream(s)... (10/30 seconds)
✓ All active streams completed
✓ Closing 3 remaining connection(s)...
✓ Server shutdown complete
```

### Timeout Shutdown

```
Initiating graceful shutdown (timeout: 10 seconds)...
✓ Stopped accepting new connections
✓ Sending GOAWAY to 2 connection(s)...
  Waiting for 5 active stream(s)... (0/10 seconds)
  Waiting for 5 active stream(s)... (5/10 seconds)
  Waiting for 2 active stream(s)... (10/10 seconds)
⚠ Timeout reached with 2 active stream(s) remaining
✓ Closing 2 remaining connection(s)...
✓ Server shutdown complete
```

## Timeout Guidelines

Choose timeout based on your RPC characteristics:

| Scenario | Recommended Timeout |
|----------|---------------------|
| Fast APIs (< 100ms) | 5-10 seconds |
| Typical APIs (100ms-1s) | 30 seconds |
| Long-running RPCs | 60-120 seconds |
| Streaming RPCs | 120+ seconds |

**Rule of thumb:** `timeout = P95_latency × 3`

## Testing Graceful Shutdown

### Manual Test

Terminal 1 - Start server:
```bash
$ sbcl --load examples/graceful-shutdown-demo.lisp
Server ready on port 50051
(Press Ctrl+C to test graceful shutdown)
```

Terminal 2 - Send requests:
```bash
$ while true; do grpcurl -plaintext localhost:50051 list; sleep 1; done
```

Terminal 1 - Press Ctrl+C:
```
^C
Shutting down...
✓ All active streams completed
✓ Server shutdown complete
```

Terminal 2 - See requests complete cleanly (no errors)

### Automated Test

```bash
$ sbcl --load tests/test-graceful-shutdown.lisp
```

## Monitoring Active Streams

You can check active streams before shutdown:

```lisp
(defun count-active-streams (server)
  (bordeaux-threads:with-lock-held ((grpc-server-lock server))
    (loop for conn across (grpc-server-connections server)
          sum (hash-table-count (http2-connection-active-calls conn)))))

;; Before shutdown
(format t "Active streams: ~D~%" (count-active-streams *server*))
```

## Best Practices

### DO ✓

- Set timeout longer than your longest expected RPC duration
- Monitor shutdown duration in production
- Use Kubernetes `terminationGracePeriodSeconds` > server timeout
- Test graceful shutdown in your CI/CD pipeline
- Log shutdown events for debugging

### DON'T ✗

- Don't set timeout too short (will drop requests)
- Don't assume all requests will complete in time
- Don't forget to handle SIGTERM in production
- Don't skip testing with real load

## Architecture

### Client Connection Pool

```
[Connection Pool]
├─ Connection 1 (active, goaway=false)    ← New requests use this
├─ Connection 2 (active, goaway=true)     ← Draining, no new streams
│  ├─ Stream 3 (completing...)
│  └─ Stream 5 (completing...)
└─ Connection 3 (closed)                   ← Will be removed
```

### Server Connection Tracking

```
[Server]
├─ Connection 1: 3 active streams
│  ├─ Stream 1: handling request...
│  ├─ Stream 3: handling request...
│  └─ Stream 5: handling request...
├─ Connection 2: 0 active streams ← Can close immediately
└─ Connection 3: 1 active stream
   └─ Stream 7: handling request...

Total active streams: 4
Shutdown will wait for these 4 to complete
```

## HTTP/2 GOAWAY Frame

The GOAWAY frame tells the client:

```
GOAWAY Frame
├─ Last Stream ID: 127        ← Streams ≤ 127 can complete
├─ Error Code: 0 (NO_ERROR)   ← Clean shutdown, not an error
└─ Debug Data: "Server shutting down"
```

After receiving GOAWAY, the client:
1. Completes streams ≤ Last Stream ID on this connection
2. Opens NEW connection for new requests
3. Closes old connection when all streams done

## Troubleshooting

### Shutdown Takes Too Long

**Symptom:** Server takes full timeout to shutdown

**Causes:**
- Streaming RPCs not closing properly
- Request handlers hanging/deadlocked
- Connection pool not releasing connections

**Fix:**
```lisp
;; Add logging to track which streams are active
(defun count-active-streams-verbose (server)
  (bordeaux-threads:with-lock-held ((grpc-server-lock server))
    (loop for conn across (grpc-server-connections server)
          do (format t "Connection ~A: ~D active streams~%"
                    conn
                    (hash-table-count (http2-connection-active-calls conn))))))
```

### Clients See Connection Errors

**Symptom:** "Connection reset" errors during shutdown

**Cause:** Timeout too short, server force-closing active requests

**Fix:** Increase timeout or investigate why requests are slow

### Server Hangs on Shutdown

**Symptom:** `stop-server` never returns

**Cause:** Bug in shutdown logic (please file an issue!)

**Workaround:** Send SIGKILL after timeout:
```bash
$ timeout 90 kill <pid> || kill -9 <pid>
```

## Comparison with Other Implementations

| Feature | ClGRPC | grpc-go | grpc-java |
|---------|--------|---------|-----------|
| Sends GOAWAY | ✓ | ✓ | ✓ |
| Configurable timeout | ✓ | ✓ | ✓ |
| Force-close after timeout | ✓ | ✓ | ✓ |
| Progress reporting | ✓ | ✗ | ✗ |
| Client auto-reconnect | ✓ | ✓ | ✓ |
| Zero-downtime deploys | ✓ | ✓ | ✓ |

## See Also

- [PERFORMANCE.md](PERFORMANCE.md) - Performance optimization guide
- [examples/graceful-shutdown-demo.lisp](examples/graceful-shutdown-demo.lisp) - Full example
- [RFC 7540](https://tools.ietf.org/html/rfc7540#section-6.8) - HTTP/2 GOAWAY frame spec
