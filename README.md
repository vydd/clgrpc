# clgrpc - Common Lisp gRPC Library

A pure Common Lisp implementation of gRPC with HTTP/2, built from scratch.

**Status**: Production Ready ✅ | **Tests**: 390/390 passing (100%) | **Interop**: Full compatibility with official gRPC

## What is clgrpc?

clgrpc is a complete gRPC library for Common Lisp that implements:

- **HTTP/2** protocol from scratch (RFC 9113 compliant)
- **Protocol Buffers** (proto3) with parser and code generator
- **gRPC Protocol** with full metadata and status code support
- **Client and Server** with all four RPC patterns (unary, client streaming, server streaming, bidirectional)
- **TLS with ALPN** for secure HTTP/2 negotiation

Unlike other implementations, clgrpc has zero gRPC-specific external dependencies - everything is built in pure Common Lisp.

## Features

- ✅ **Complete gRPC Implementation**: All four RPC patterns supported
- ✅ **Full HTTP/2**: HPACK compression, Huffman encoding, stream multiplexing, flow control
- ✅ **Custom Protocol Buffers**: Proto3 implementation with parser and code generator (~640 lines)
- ✅ **Secure Communication**: TLS with ALPN negotiation ("h2")
- ✅ **Interoperability**: Fully compatible with official gRPC (verified with Go implementation)
- ✅ **High Performance**: Buffered I/O (8KB buffers), efficient encoding/decoding
- ✅ **Well Tested**: 390 comprehensive unit tests, full integration testing

## Quick Start

### Prerequisites

- **Common Lisp**: SBCL (recommended), other implementations may work
- **Operating System**: Linux, macOS (tested), Windows (untested but should work)
- **Quicklisp**: For dependency management

### Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/clgrpc.git ~/Code/clgrpc
cd ~/Code/clgrpc

# Install dependencies (one-time setup)
sbcl --eval '(ql:quickload :usocket)' \
     --eval '(ql:quickload :cl+ssl)' \
     --eval '(ql:quickload :bordeaux-threads)' \
     --eval '(ql:quickload :babel)' \
     --quit

# Load and test the system
sbcl --eval '(asdf:load-system :clgrpc)' \
     --eval '(asdf:test-system :clgrpc)' \
     --quit
```

Expected output: `390/390 tests passing (100%)`

### Hello World Example

**Server:**
```lisp
(use-package :clgrpc.server)

;; Define a handler
(defclass greeter-handler () ())

(defmethod handle-unary ((handler greeter-handler) service method request-bytes context)
  (declare (ignore service context))
  (when (string= method "SayHello")
    (let* ((request (proto-deserialize 'hello-request request-bytes))
           (name (hello-request-name request))
           (reply (make-hello-reply :message (format nil "Hello ~A!" name))))
      (values (proto-serialize reply)
              +grpc-status-ok+ nil nil))))

;; Create and start server
(let ((server (make-server :port 50051)))
  (register-handler (grpc-server-router server)
                    "helloworld.Greeter" "SayHello"
                    (make-instance 'greeter-handler)
                    :rpc-type :unary)
  (start-server server)
  (format t "Server listening on port 50051~%")
  (loop (sleep 1)))  ; Keep running
```

**Client:**
```lisp
(use-package :clgrpc.client)

;; Create a channel and make a call
(let ((channel (make-channel "localhost:50051" :secure nil)))
  (unwind-protect
       (let* ((request (make-hello-request :name "World"))
              (request-bytes (proto-serialize request)))
         (multiple-value-bind (response-bytes status status-message)
             (call-unary channel "helloworld.Greeter" "SayHello" request-bytes)
           (if (= status +grpc-status-ok+)
               (let ((reply (proto-deserialize 'hello-reply response-bytes)))
                 (format t "Response: ~A~%" (hello-reply-message reply)))
               (format t "Error: ~A (~D)~%" status-message status))))
    (close-channel channel)))
```

## Requirements

### Common Lisp Implementations

- **SBCL** (recommended, fully tested)
- Other implementations may work but are untested

### Operating Systems

- **Linux** (fully tested)
- **macOS** (tested)
- **Windows** (untested but should work with minor adjustments)

### Dependencies

Runtime dependencies (installed via Quicklisp):
- **usocket** - Portable TCP sockets
- **cl+ssl** - TLS/SSL with ALPN support
- **bordeaux-threads** - Threading
- **babel** - String encoding
- **alexandria** - Utility functions
- **fast-io** - Efficient binary I/O

Test dependencies:
- **fiveam** - Unit testing framework

## Usage

### Creating a Client

```lisp
(use-package :clgrpc.client)

;; Create a channel (insecure connection)
(defvar *channel* (make-channel "localhost:50051" :secure nil))

;; Create a channel (secure connection)
(defvar *secure-channel* (make-channel "example.com:443" :secure t))

;; Make a unary call
(multiple-value-bind (response-bytes status status-message)
    (call-unary *channel* "myservice.MyService" "MyMethod" request-bytes
                :timeout 5000  ; milliseconds
                :metadata '(("custom-header" . "value")))
  (when (= status clgrpc.grpc:+grpc-status-ok+)
    (process-response response-bytes)))

;; Always close the channel when done
(close-channel *channel*)
```

### Creating a Server

```lisp
(use-package :clgrpc.server)

;; Define a handler class
(defclass my-handler () ())

;; Implement the handler method
(defmethod handle-unary ((handler my-handler) service method request-bytes context)
  (declare (ignore service context))
  (let ((response-bytes (process-request request-bytes)))
    (values response-bytes +grpc-status-ok+ nil nil)))

;; Create and configure server
(defvar *server* (make-server :port 50051))

;; Register service handlers
(register-handler (grpc-server-router *server*)
                  "myservice.MyService" "MyMethod"
                  (make-instance 'my-handler)
                  :rpc-type :unary)

;; Start the server
(start-server *server*)

;; Stop when done
(stop-server *server*)
```

### Streaming RPCs

clgrpc supports all four gRPC streaming patterns:

1. **Unary**: Client sends one, server sends one (shown above)
2. **Client Streaming**: Client sends many, server sends one
3. **Server Streaming**: Client sends one, server sends many
4. **Bidirectional**: Both send many

See `examples/routeguide/` for complete streaming examples.

### Protocol Buffers

clgrpc includes a complete proto3 implementation with a parser and code generator:

```lisp
;; Generate Common Lisp code from .proto file
(compile-proto-file "my-service.proto" "generated.lisp")

;; Load the generated code
(load "generated.lisp")

;; Use the generated encoder/decoder functions
(let ((bytes (encode-my-message name age email)))
  (multiple-value-bind (name age email)
      (decode-my-message bytes)
    (format t "Name: ~A, Age: ~D~%" name age)))
```

## Technical Overview

clgrpc is built in layers, from low-level networking to high-level gRPC APIs:

```
┌─────────────────────────────────────────┐
│         gRPC Client/Server API          │ ← call-unary, register-handler
├─────────────────────────────────────────┤
│      gRPC Protocol Layer                │ ← Message framing, status codes
├─────────────────────────────────────────┤
│     Protocol Buffers (Custom)           │ ← proto3 wire format
├─────────────────────────────────────────┤
│        HTTP/2 Connection                │ ← Stream multiplexing
├─────────────────────────────────────────┤
│       HTTP/2 Frames + HPACK             │ ← Binary protocol
├─────────────────────────────────────────┤
│       Buffered I/O (8KB buffers)        │ ← Efficient I/O
├─────────────────────────────────────────┤
│      TLS (cl+ssl) | TCP (usocket)       │ ← Network transport
└─────────────────────────────────────────┘
```

Each layer is independently tested with comprehensive unit tests. The entire stack is approximately 6,200 lines of well-documented Common Lisp code.

For detailed technical information, see [DEVELOPMENT.md](DEVELOPMENT.md).

## Testing

```bash
# Run all unit tests (390 tests)
sbcl --load run-tests.lisp

# Run interop tests (test against official Go gRPC)
cd tests/interop
./setup.sh  # First time only
./test.sh
```

All tests should pass with output: `390/390 tests passing (100%)`

## Examples

- **HelloWorld**: Basic unary RPC example
- **RouteGuide**: Comprehensive example demonstrating all four streaming patterns
  - `GetFeature`: Unary RPC
  - `ListFeatures`: Server streaming RPC
  - `RecordRoute`: Client streaming RPC
  - `RouteChat`: Bidirectional streaming RPC

See the `examples/` directory for complete working examples.

## Roadmap

### Current Status (v0.9)
- ✅ Complete HTTP/2 implementation
- ✅ Complete Protocol Buffers implementation
- ✅ Full gRPC protocol support
- ✅ Client and server with all RPC patterns
- ✅ Full interoperability with official gRPC

### Upcoming (v1.0)
- [ ] Performance optimization and profiling
- [ ] Message compression (gzip)
- [ ] Health checking service
- [ ] Enhanced error handling and debugging
- [ ] Connect conformance suite testing

### Future
- [ ] Connection health checking (PING/PONG)
- [ ] Retry policies and load balancing
- [ ] Interceptors/middleware
- [ ] Additional platform support (Windows, other Lisps)

See [DEVELOPMENT.md](DEVELOPMENT.md) for detailed development roadmap.

## Performance

clgrpc is designed for performance with:

- **Buffered I/O**: 8KB buffers reduce syscalls significantly
- **HPACK Compression**: 30-70% header compression
- **Huffman Encoding**: Additional 10-30% compression on text
- **Zero-Copy Operations**: Direct byte array operations
- **Stream Multiplexing**: Multiple RPCs over single connection

Performance benchmarks against official implementations are planned.

## Contributing

Contributions are welcome! Areas for contribution:

- **Performance optimization**: Profiling and optimization
- **Additional features**: Compression, health checking, interceptors
- **Platform support**: Windows, other Common Lisp implementations
- **Documentation**: Tutorials, examples, API reference
- **Bug reports and fixes**: Issue tracking and bug fixes

Please open an issue to discuss significant changes before submitting a pull request.

## License

MIT License

Copyright (c) 2026 clgrpc contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

## References

- [gRPC Official Website](https://grpc.io/)
- [RFC 9113: HTTP/2](https://www.rfc-editor.org/rfc/rfc9113.html)
- [RFC 7541: HPACK Header Compression](https://httpwg.org/specs/rfc7541.html)
- [gRPC Protocol Specification](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md)
- [Protocol Buffers Encoding](https://developers.google.com/protocol-buffers/docs/encoding)
- [gRPC Status Codes](https://grpc.io/docs/guides/status-codes/)

## Acknowledgments

Built from scratch for the Common Lisp community. Special thanks to:
- The authors of usocket, cl+ssl, and bordeaux-threads for foundational libraries
- The gRPC and HTTP/2 specification authors
- The Common Lisp community for feedback and support
