#!/bin/bash
# Test CL gRPC server with Go client

set -e

echo "=== Testing CL gRPC Server with Go Client ==="
echo ""

# Change to interop directory
cd "$(dirname "$0")"

# Start CL server in background
echo "Starting CL gRPC server..."
sbcl --script cl-server/server.lisp --run 2>&1 | tee server.log &
SERVER_PID=$!

# Wait for server to start
echo "Waiting for server to start..."
sleep 3

# Call server with Go client
echo ""
echo "Calling server with Go client..."
./bin/go-client -addr=localhost:50051 -name="CL Server Test" 2>&1

# Capture result
RESULT=$?

# Stop server
echo ""
echo "Stopping server..."
kill $SERVER_PID 2>/dev/null || true
wait $SERVER_PID 2>/dev/null || true

# Check result
if [ $RESULT -eq 0 ]; then
    echo ""
    echo "✓ SUCCESS: CL server handled Go client request!"
    exit 0
else
    echo ""
    echo "✗ FAIL: Test failed with exit code $RESULT"
    echo ""
    echo "Server log:"
    cat server.log
    exit 1
fi
