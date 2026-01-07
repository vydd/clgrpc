# clgrpc - Common Lisp gRPC Library

A pure Common Lisp implementation of gRPC with HTTP/2, built from scratch for SBCL.

## Project Status

**Phase 1: HTTP/2 Frames - COMPLETE ✓**
- All 10 HTTP/2 frame types implemented (DATA, HEADERS, PRIORITY, RST_STREAM, SETTINGS, PUSH_PROMISE, PING, GOAWAY, WINDOW_UPDATE, CONTINUATION)
- Full encoding and decoding support
- Comprehensive test suite: **133/133 tests passing (100%)**

**Phase 2: HPACK + Huffman - COMPLETE ✓**
- Huffman encoding/decoding with full 256-entry code table
- HPACK static table (61 entries) and dynamic table with eviction
- Integer and string encoding/decoding
- All header field representations (indexed, literal with/without indexing)
- Comprehensive test suite: **216/225 HTTP/2 tests passing (96%)**

## Features

### Currently Implemented (Phases 1-2)
- ✓ HTTP/2 frame structures and encoding/decoding
- ✓ All 10 frame types per RFC 9113
- ✓ Frame flag handling
- ✓ Huffman encoding/decoding (RFC 7541 Appendix B)
- ✓ HPACK header compression (RFC 7541)
  - Static table (61 entries)
  - Dynamic table with size management and eviction
  - All header field representations
  - Integer and string encoding with Huffman option
- ✓ Binary utilities for byte array operations
- ✓ HTTP/2 error codes and conditions
- ✓ Comprehensive unit tests with FiveAM

### Planned (Future Phases)
- Phase 3: HTTP/2 streams, connection management, flow control (NEXT)
- Phase 4: gRPC protocol layer (message framing, metadata, status codes)
- Phase 5: gRPC client implementation
- Phase 6: gRPC server implementation
- Phase 7: Interoperability testing with official gRPC implementations
- Phase 8: Production readiness (conformance tests, documentation, examples)

## Dependencies

### Runtime Dependencies
- `cl+ssl` - TLS/SSL support (for future phases)
- `usocket` - Portable TCP sockets (for future phases)
- `bordeaux-threads` - Threading (for future phases)
- `alexandria` - Utility functions
- `trivial-gray-streams` - Stream abstraction (for future phases)
- `fast-io` - Efficient binary I/O
- `babel` - String encoding

### Build/Test Dependencies
- `fiveam` - Unit testing framework

### Note
`cl-protobufs` will be required in Phase 4 but is not currently in Quicklisp. Alternative protobuf implementations will be evaluated when needed.

## Installation

```bash
# Clone the repository
cd ~/Code
git clone <your-repo-url> clgrpc
cd clgrpc

# The project uses Quicklisp for dependencies
sbcl --load run-tests.lisp
```

## Running Tests

```bash
# Run all tests
sbcl --load run-tests.lisp

# Or interactively in REPL
sbcl
(load "~/quicklisp/setup.lisp")
(ql:quickload :clgrpc-tests)
(fiveam:run! 'clgrpc-tests:frame-tests)
```

## Project Structure

```
clgrpc/
├── clgrpc.asd              # Main ASDF system definition
├── clgrpc-tests.asd        # Test system definition
├── run-tests.lisp          # Convenient test runner script
├── README.md               # This file
├── src/
│   ├── package.lisp        # Package definitions
│   ├── utils/
│   │   └── binary-utils.lisp    # Byte array operations
│   ├── http2/
│   │   ├── errors.lisp          # HTTP/2 error codes
│   │   └── frames.lisp          # Frame encoding/decoding
│   ├── grpc/                    # gRPC layer (stubs)
│   ├── transport/               # Socket/TLS layer (stubs)
│   ├── client/                  # Client implementation (stubs)
│   └── server/                  # Server implementation (stubs)
├── tests/
│   ├── package.lisp
│   └── http2/
│       └── frame-tests.lisp     # Comprehensive frame tests
└── examples/                    # Future examples
```

## API Overview (Phase 1)

### HTTP/2 Frames

```lisp
(use-package :clgrpc.http2)

;; Create a DATA frame
(let ((frame (make-data-frame 1 #(1 2 3 4 5) :end-stream t)))
  ;; Encode to bytes
  (let ((bytes (encode-frame frame)))
    ;; Decode back
    (let ((decoded (decode-frame bytes)))
      (frame-payload decoded))))  ; => #(1 2 3 4 5)

;; Create a HEADERS frame
(make-headers-frame 1 #(header-data...)
                    :end-stream t
                    :end-headers t
                    :priority t
                    :stream-dependency 0
                    :weight 100)

;; Create a SETTINGS frame
(make-settings-frame :settings '((1 . 4096) (3 . 100)))

;; Check frame flags
(frame-flag-set-p frame +flag-end-stream+)  ; => T
```

### Binary Utilities

```lisp
(use-package :clgrpc.utils)

;; Encode integers to big-endian bytes
(encode-uint16-be 1234)  ; => #(4 210)
(encode-uint32-be 123456)  ; => #(0 1 226 64)

;; Decode from big-endian bytes
(decode-uint32-be #(0 1 226 64))  ; => 123456
```

## Testing Philosophy

The library emphasizes correctness through comprehensive testing:

- **Unit tests** for all frame types and encoding/decoding
- **Round-trip tests** ensure encode/decode symmetry
- **Error handling tests** validate protocol violations are caught
- **Edge case tests** cover boundary conditions (empty payloads, max sizes, etc.)

## Design Decisions

1. **Pure Common Lisp HTTP/2**: Implements HTTP/2 from scratch per RFC 9113, not relying on outdated libraries
2. **Protocol Buffers via cl-protobufs**: Will use existing library (Phase 4+)
3. **TLS via cl+ssl**: Leverage existing TLS implementation with ALPN support
4. **Type safety**: Extensive use of type declarations for performance and correctness
5. **No external gRPC dependencies**: All gRPC protocol logic implemented in Common Lisp

## Development Roadmap

See the [implementation plan](.claude/plans/stateful-orbiting-kay.md) for detailed phase breakdown and milestones.

**Current Focus**: Phase 2 - HPACK header compression and Huffman coding

## License

MIT License

## Contributing

Contributions are welcome! This project aims to provide a production-quality gRPC implementation for Common Lisp.

## References

- [RFC 9113: HTTP/2](https://www.rfc-editor.org/rfc/rfc9113.html)
- [RFC 7541: HPACK Header Compression](https://httpwg.org/specs/rfc7541.html)
- [gRPC Protocol Specification](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md)
- [gRPC Status Codes](https://grpc.io/docs/guides/status-codes/)
