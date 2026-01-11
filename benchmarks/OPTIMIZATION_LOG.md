# Performance Optimization Log

## Baseline (with ALL debug output)
**Date:** 2026-01-11 (before any changes)
- 1 client: 12 req/s
- 10 clients: 120 req/s
- 100 clients: 1,179 req/s

## After Frame I/O debug-log wrapping (SEND:/RECV: suppressed)
**Changes:** Wrapped frame-writer.lisp and frame-reader.lisp debug output in debug-log macro
**Date:** 2026-01-11
- 1 client: 11.4 req/s (-4.8%)
- 10 clients: 122.0 req/s (+1.7%)
- 100 clients: 1,173.4 req/s (-0.5%)

**Analysis:** No significant performance improvement yet. Other debug output still active:
- Frame reader thread messages
- Connection pool debug
- Server processing debug
- Call registration debug

Need to wrap ALL debug output.

## After ALL debug output suppressed
**Changes:** Wrapped ALL debug output in debug-log macro (frame I/O, connection pool, server, client, RouteGuide example)
**Date:** 2026-01-11  
- 1 client: 11.6 req/s (-3.3% from baseline)
- 10 clients: 120.6 req/s (+0.5% from baseline)
- 100 clients: 1,178.0 req/s (-0.08% from baseline)

**Analysis:** **NO significant performance improvement from removing debug logging!**

This is a critical finding - debug logging was NOT the bottleneck. Performance is essentially identical to baseline (within measurement noise).

**Implications:**
- The real bottlenecks are NOT in debug output
- Must look elsewhere for performance improvements:
  - Frame encoding/decoding
  - HPACK header compression
  - Protobuf serialization
  - Memory allocation/GC
  - Lock contention
  - Syscalls

**Next steps:** Profile with SBCL's statistical profiler to identify actual hot paths.

**Comparison with Go (baseline):**
- CL 1 client: 11.6 req/s vs Go 9,214 req/s = 0.13% (794x slower)
- CL 10 clients: 120.6 req/s vs Go 30,850 req/s = 0.39% (256x slower)  
- CL 100 clients: 1,178 req/s vs Go 78,471 req/s = 1.50% (67x slower)
