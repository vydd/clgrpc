#!/bin/bash
# Test: Start debug server, send 2 requests, see what breaks

set -e

echo "═══════════════════════════════════════════════════════════"
echo "  Debug Test: 2 Requests to CL Server"
echo "═══════════════════════════════════════════════════════════"
echo ""

# Clean up
pkill -9 sbcl 2>/dev/null || true
sleep 2

cd /home/vydd/Code/clgrpc/benchmarks

# Build Go test client
echo "Building Go test client..."
go build -o test-client test-two-requests.go

# Start CL server with debug logging
echo "Starting CL server with DEBUG logging..."
sbcl --noinform --load debug-server.lisp > /tmp/debug-server.log 2>&1 &
SERVER_PID=$!

# Wait for server
echo "Waiting for server to start..."
for i in {1..20}; do
    if lsof -i :50055 2>/dev/null | grep -q LISTEN; then
        echo "✓ Server is ready"
        break
    fi
    sleep 1
    if [ $i -eq 20 ]; then
        echo "✗ Server failed to start"
        cat /tmp/debug-server.log
        kill $SERVER_PID 2>/dev/null || true
        exit 1
    fi
done

sleep 2

echo ""
echo "─────────────────────────────────────────────────────────────"
echo "Sending 2 requests from Go client..."
echo "─────────────────────────────────────────────────────────────"
echo ""

# Run the Go client
./test-client

echo ""
echo "─────────────────────────────────────────────────────────────"
echo "Server Debug Output:"
echo "─────────────────────────────────────────────────────────────"
cat /tmp/debug-server.log

# Stop server
echo ""
echo "Stopping server..."
kill $SERVER_PID 2>/dev/null || true

echo ""
echo "Done! Check the debug output above to see what happened."
