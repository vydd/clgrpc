# Performance Results

## Current Status (After Optimizations)

### Sequential Performance (1 Client, 10 Requests)

**Before Optimizations:**
- Request 1: 54ms
- Requests 2-10: ~41ms each
- Average: ~42ms per request
- Rate: ~24 req/sec

**After Optimizations:**
- Request 1: 19.5ms  
- Requests 2-10: ~350-400µs each
- Average: ~2.3ms per request  
- Rate: ~435 req/sec

**Improvement: 18x faster overall, 100x faster for subsequent requests**

## Optimizations Applied

### 1. Fixed HPACK Pseudo-Header Bug
**Problem:** Dynamic table stored pseudo-headers (`:path`, `:method`, etc.) as lowercase strings but didn't convert them back to keywords when retrieved.

**Impact:** 99% error rate → 0% errors (made server functional)

**Fix:** Added `denormalize-header-name` function in `src/http2/hpack.lisp`

### 2. Batched Frame Writes  
**Problem:** Flushing after every single HTTP/2 frame (3 flushes per unary RPC: HEADERS, DATA, TRAILERS)

**Impact:** 17% improvement for single client (24 → 28 rps)

**Fix:** Modified `write-frame-to-stream` to support batching, updated `server-send-response` to batch all 3 frames with single flush

### 3. TCP_NODELAY ⭐ **MASSIVE WIN**
**Problem:** Nagle's algorithm + delayed ACK interaction caused 40ms delay per response

**Impact:** 100x improvement on subsequent requests (41ms → 0.35ms)

**Fix:** Set `TCP_NODELAY` on all sockets (client and server) in `src/transport/socket.lisp`

## Comparison with Go Server

**Go Server (Sequential, 1 Client):**
- Request 1: 2.2ms
- Requests 2-10: 100-200µs

**CL Server (Sequential, 1 Client):**  
- Request 1: 19.5ms
- Requests 2-10: 350-400µs

**Gap:** CL is still 2-4x slower, but vastly improved from original 40ms delay

## Next Steps

- Profile remaining overhead (first request: 19ms vs 2ms)
- Test and optimize concurrent request handling
- Investigate connection pooling and reuse
