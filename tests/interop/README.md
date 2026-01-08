# gRPC Interoperability Tests

This directory contains interoperability tests between the Common Lisp gRPC implementation (clgrpc) and the official Go gRPC implementation.

## Structure

- `proto/` - Protocol Buffer definitions
- `go-server/` - Go gRPC server implementation
- `go-client/` - Go gRPC client implementation
- `cl-server/` - Common Lisp gRPC server implementation
- `cl-client/` - Common Lisp gRPC client implementation

## Setup

### Prerequisites

- Go 1.21 or later
- Protocol Buffers compiler (`protoc`)
- SBCL (Steel Bank Common Lisp)

### Build Go Components

```bash
./setup.sh
```

This will:
1. Install Go protoc plugins
2. Generate Go protobuf code
3. Download Go dependencies
4. Build Go server and client binaries

## Running Tests

### Test 1: Go Server + CL Client

**Terminal 1** (Start Go server):
```bash
./bin/go-server
```

**Terminal 2** (Run CL client):
```bash
sbcl --load cl-client/client.lisp
```

### Test 2: CL Server + Go Client

**Terminal 1** (Start CL server):
```bash
sbcl --load cl-server/server.lisp
```

**Terminal 2** (Run Go client):
```bash
./bin/go-client
```

### Test 3: Go-to-Go (Baseline)

**Terminal 1**:
```bash
./bin/go-server
```

**Terminal 2**:
```bash
./bin/go-client
```

### Test 4: CL-to-CL

**Terminal 1**:
```bash
sbcl --load cl-server/server.lisp
```

**Terminal 2**:
```bash
sbcl --load cl-client/client.lisp
```

## Expected Behavior

All test combinations should successfully exchange HelloWorld greetings with the response "Hello <name>".

## Troubleshooting

- **Go server won't start**: Check if port 50051 is already in use (`lsof -i :50051`)
- **Connection refused**: Ensure the server is running before starting the client
- **Protoc not found**: Install protobuf compiler for your OS
- **Go dependencies fail**: Try `go mod tidy` in this directory
