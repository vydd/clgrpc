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
- ✅ **CLOS-based Service API**: Type-safe, automatic serialization, clean syntax matching Go/Python gRPC
- ✅ **Client Stub Generation**: Automatic type-safe client generation from service definitions
- ✅ **CLOS Protocol Buffers**: Proto3 messages as CLOS objects with metaclass
- ✅ **Full HTTP/2**: HPACK compression, Huffman encoding, stream multiplexing, flow control
- ✅ **Code Generation**: .proto file parser and CLOS code generator (~640 lines)
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

**Server (CLOS API - Recommended):**
```lisp
(use-package :clgrpc.grpc)
(use-package :clgrpc.server)

;; Define a service using CLOS
(defclass greeter-service (grpc-service)
  ()
  (:metaclass grpc-service-metaclass)
  (:service-name "helloworld.Greeter")
  (:package "helloworld"))

;; Define a method - automatic name conversion and serialization!
(defgrpc-method say-hello ((service greeter-service)
                           (request hello-request)
                           context)
  (declare (ignore service context))
  (let ((name (hello-request-name request)))
    (make-hello-reply :message (format nil "Hello ~A!" name))))

;; Create and start server
(let ((server (make-server :port 50051)))
  ;; Automatic registration of all methods
  (register-service (grpc-server-router server)
                    (make-instance 'greeter-service))
  (start-server server)
  (format t "Server listening on port 50051~%")
  (loop (sleep 1)))  ; Keep running
```

**Client (with Stub - Recommended):**
```lisp
(use-package :clgrpc.client)
(use-package :clgrpc.grpc)

;; Generate type-safe stub from server service definition
(defstub greeter-stub clgrpc.grpc::greeter-service)

;; Create a channel and make calls
(let ((channel (make-channel "localhost:50051" :secure nil)))
  (unwind-protect
       (let ((stub (make-instance 'greeter-stub :channel channel)))
         ;; Clean syntax - automatic serialization!
         (let ((reply (say-hello stub (make-hello-request :name "World"))))
           (format t "Response: ~A~%" (hello-reply-message reply))))
    (close-channel channel)))
```

**Client (Low-Level API):**
```lisp
(use-package :clgrpc.client)
(use-package :clgrpc.grpc)

;; Create a channel and make a call
(let ((channel (make-channel "localhost:50051" :secure nil)))
  (unwind-protect
       (let ((request (make-hello-request :name "World")))
         (multiple-value-bind (response-bytes status status-message)
             (call-unary channel "helloworld.Greeter" "SayHello"
                        (proto-serialize request))
           (if (= status +grpc-status-ok+)
               (let ((reply (proto-deserialize 'hello-reply response-bytes)))
                 (format t "Response: ~A~%" (hello-reply-message reply)))
               (format t "Error: ~A (~D)~%" status-message status))))
    (close-channel channel)))
```

**Key Features of CLOS API:**
- ✅ **Client Stubs**: Automatic generation from service definitions
- ✅ **Type-Safe**: No manual string matching - compile-time dispatch
- ✅ **Automatic Serialization**: Both client and server handle serialization
- ✅ **Smart Defaults**: `say-hello` → `"SayHello"`, `:unary` default
- ✅ **Single Registration**: One call per service registers all methods
- ✅ **Clean Syntax**: Matching Go/Python gRPC style

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

**With Client Stubs (Recommended):**

Client stubs provide automatic type-safe client generation from service definitions:

```lisp
(use-package :clgrpc.client)
(use-package :clgrpc.grpc)

;; Generate a stub from a CLOS service definition
;; This introspects the service class and generates type-safe methods
(defstub my-service-stub clgrpc.grpc::my-service)

;; Create a channel and stub instance
(defvar *channel* (make-channel "localhost:50051" :secure nil))
(defvar *stub* (make-instance 'my-service-stub :channel *channel*))

;; Make calls with automatic serialization - clean syntax!
(let ((response (my-method *stub*
                           (make-my-request :field "value")
                           :timeout 5000
                           :metadata '(("custom-header" . "value")))))
  (format t "Result: ~A~%" (my-response-result response)))

;; Always close the channel when done
(close-channel *channel*)
```

**Key Benefits of Stubs:**
- ✅ Type-safe method calls - no string names
- ✅ Automatic serialization/deserialization
- ✅ Compile-time method validation
- ✅ Clean, idiomatic syntax
- ✅ Works with any CLOS service definition

**Low-Level Client API:**

For advanced use cases, you can use the low-level API directly:

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

**CLOS API (Recommended):**

```lisp
(use-package :clgrpc.grpc)
(use-package :clgrpc.server)

;; Define a service class
(defclass my-service (grpc-service)
  ()
  (:metaclass grpc-service-metaclass)
  (:service-name "myservice.MyService")
  (:package "myservice"))

;; Define methods - automatic name conversion and serialization
(defgrpc-method my-method ((service my-service)
                          (request my-request)
                          context)
  (declare (ignore service context))
  ;; Process the request (already deserialized!)
  (let ((result (process-request request)))
    ;; Return response object (auto-serialized!)
    (make-my-response :result result)))

;; Create and configure server
(defvar *server* (make-server :port 50051))

;; Register entire service (all methods automatically registered)
(register-service (grpc-server-router *server*)
                  (make-instance 'my-service))

;; Start the server
(start-server *server*)

;; Stop when done
(stop-server *server*)
```

**Low-Level Handler API (Advanced):**

For fine-grained control, you can use the low-level handler API:

```lisp
(use-package :clgrpc.server)

;; Define a handler class
(defclass my-handler () ())

;; Implement the handler method
(defmethod handle-unary ((handler my-handler) service method request-bytes context)
  (declare (ignore service context))
  (let ((response-bytes (process-request request-bytes)))
    (values response-bytes +grpc-status-ok+ nil nil)))

;; Register individual handlers
(register-handler (grpc-server-router *server*)
                  "myservice.MyService" "MyMethod"
                  (make-instance 'my-handler)
                  :rpc-type :unary)
```

### Streaming RPCs

clgrpc supports all four gRPC streaming patterns with the CLOS API:

1. **Unary**: Client sends one, server sends one (shown above)
2. **Client Streaming**: Client sends many, server sends one
3. **Server Streaming**: Client sends one, server sends many
4. **Bidirectional**: Both send many

**Example - Server Streaming:**

```lisp
(defgrpc-method list-features ((service route-guide-service)
                               (request rectangle)
                               context)
  (:rpc-type :server-streaming)

  (let ((stream (get-stream context)))
    (dolist (feature *features*)
      (when (in-range? feature request)
        ;; Send each feature to client
        (server-stream-send stream (proto-serialize feature))))

    (values +grpc-status-ok+ nil nil)))
```

**Example - Client Streaming:**

```lisp
(defgrpc-method record-route ((service route-guide-service)
                              (request point)  ; Unused for streaming
                              context)
  (:rpc-type :client-streaming)
  (:response-type route-summary)

  (let ((stream (get-stream context))
        (points nil))
    ;; Receive all points from client
    (loop for msg-bytes = (server-stream-recv stream)
          while msg-bytes
          do (push (proto-deserialize 'point msg-bytes) points))

    ;; Return summary
    (values (proto-serialize (make-route-summary :count (length points)))
            +grpc-status-ok+ nil nil)))
```

See `examples/routeguide/server-clos.lisp` for complete streaming examples including bidirectional streaming.

### Protocol Buffers

clgrpc includes a complete proto3 implementation with CLOS-based messages and code generation:

**CLOS Message API (Recommended):**

```lisp
;; Define messages using CLOS
(defclass person (proto-message)
  ((name
    :initarg :name
    :accessor person-name
    :field 1
    :proto-type :string)
   (age
    :initarg :age
    :accessor person-age
    :field 2
    :proto-type :int32))
  (:metaclass proto-metaclass))

;; Use the messages
(let* ((person (make-instance 'person :name "Alice" :age 30))
       (bytes (proto-serialize person)))
  (let ((decoded (proto-deserialize 'person bytes)))
    (format t "Name: ~A, Age: ~D~%"
            (person-name decoded)
            (person-age decoded))))
```

**Code Generation from .proto:**

```lisp
;; Generate CLOS-based Common Lisp code from .proto file
(compile-proto-file "my-service.proto" "generated.lisp")

;; Load the generated code (includes CLOS classes)
(load "generated.lisp")

;; Use the generated classes
(let ((person (make-person :name "Alice" :age 30)))
  (format t "Name: ~A~%" (person-name person)))
```

The code generator produces clean CLOS-based code that can be used directly or serve as a reference for hand-written definitions.

## Technical Overview

clgrpc is built in layers, from low-level networking to high-level gRPC APIs:

```
┌─────────────────────────────────────────┐
│    Client Stubs (defstub) + CLOS API   │ ← Type-safe, auto-serialization
├─────────────────────────────────────────┤
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
  - `server-clos.lisp` - CLOS-based server (recommended)
  - `client-stub.lisp` - Client stub generation (recommended)
  - `client-clos.lisp` - CLOS-based client with message API
- **RouteGuide**: Comprehensive example demonstrating all four streaming patterns
  - `server-clos.lisp` - CLOS-based server with all RPC types (recommended)
  - `server.lisp` - Low-level handler-based server (advanced)
  - See `CLOS-COMPARISON.md` for side-by-side comparison
  - Methods:
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
- ✅ Client stub generation from service definitions
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
