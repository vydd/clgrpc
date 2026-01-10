#!/bin/bash
# Test gRPC server reflection

set -e

cd "$(dirname "$0")"

echo "=== Testing gRPC Server Reflection ==="
echo ""

# Clean up any existing server on port 50051
lsof -ti:50051 | xargs -r kill -9 2>/dev/null || true
sleep 1

# Start CL server in background
echo "Starting CL gRPC server with reflection..."
sbcl --script cl-server/server.lisp --run > /tmp/reflection-test.log 2>&1 &
SERVER_PID=$!

# Wait for server to start
echo "Waiting for server to start..."
sleep 4

# Test reflection - list services
echo ""
echo "Testing: List all services"
echo "Command: grpcurl -plaintext localhost:50051 list"
echo ""
if grpcurl -plaintext localhost:50051 list; then
    echo ""
    echo "✓ Reflection service working!"
    RESULT=0
else
    echo ""
    echo "✗ Reflection test failed"
    echo ""
    echo "Server log:"
    cat /tmp/reflection-test.log
    RESULT=1
fi

# Stop server
echo ""
echo "Stopping server..."
kill $SERVER_PID 2>/dev/null || true
wait $SERVER_PID 2>/dev/null || true

exit $RESULT
