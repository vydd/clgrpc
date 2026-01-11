#!/bin/bash
# Profile 10 sequential requests from 1 client

set -e

echo "═══════════════════════════════════════════════════════════"
echo "  Profile: 1 Client, 10 Sequential Requests"
echo "═══════════════════════════════════════════════════════════"
echo ""

cd /home/vydd/Code/clgrpc/benchmarks

# Clean up
pkill -9 sbcl 2>/dev/null || true
sleep 2

# Build Go client
echo "Building Go client..."
go build -o test-10-requests test-10-requests.go
echo ""

# Start CL server with profiler
echo "Starting CL server with profiler..."
sbcl --noinform --load profile-10-requests.lisp > /tmp/profile-output.log 2>&1 &
SERVER_PID=$!

# Wait for server
echo "Waiting for server to start..."
for i in {1..30}; do
    if lsof -i :50055 2>/dev/null | grep -q LISTEN; then
        echo "✓ Server is ready"
        break
    fi
    sleep 1
    if [ $i -eq 30 ]; then
        echo "✗ Server failed to start"
        cat /tmp/profile-output.log
        kill $SERVER_PID 2>/dev/null || true
        exit 1
    fi
done

sleep 2

echo ""
echo "─────────────────────────────────────────────────────────────"
echo "Running 10 requests..."
echo "─────────────────────────────────────────────────────────────"
./test-10-requests

echo ""
echo "Waiting for profiler to finish and generate report..."

# Wait for SBCL to finish (it will exit after generating report)
wait $SERVER_PID 2>/dev/null || true

echo ""
echo "═══════════════════════════════════════════════════════════"
echo "  PROFILER OUTPUT"
echo "═══════════════════════════════════════════════════════════"
cat /tmp/profile-output.log

echo ""
echo "Done!"
