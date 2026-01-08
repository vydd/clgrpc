#!/bin/bash
# Setup script for Go interop tests

set -e

echo "Setting up Go gRPC interop tests..."

# Check if protoc is installed
if ! command -v protoc &> /dev/null; then
    echo "Error: protoc not found. Please install protobuf compiler."
    echo "  On Ubuntu/Debian: sudo apt-get install protobuf-compiler"
    echo "  On macOS: brew install protobuf"
    exit 1
fi

# Check if Go is installed
if ! command -v go &> /dev/null; then
    echo "Error: Go not found. Please install Go 1.21 or later."
    exit 1
fi

cd "$(dirname "$0")"

# Install Go protoc plugins
echo "Installing Go protoc plugins..."
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest

# Generate Go protobuf code
echo "Generating Go protobuf code..."
mkdir -p proto
protoc --go_out=. --go_opt=paths=source_relative \
    --go-grpc_out=. --go-grpc_opt=paths=source_relative \
    proto/helloworld.proto

# Download Go dependencies
echo "Downloading Go dependencies..."
go mod download
go mod tidy

# Build Go server
echo "Building Go server..."
go build -o bin/go-server ./go-server/main.go

# Build Go client
echo "Building Go client..."
go build -o bin/go-client ./go-client/main.go

echo "Setup complete!"
echo ""
echo "To run the Go server:"
echo "  ./bin/go-server"
echo ""
echo "To run the Go client (in another terminal):"
echo "  ./bin/go-client"
