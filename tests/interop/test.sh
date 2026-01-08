#!/bin/bash
# Test script for running interop tests

set -e

cd "$(dirname "$0")"

echo "gRPC Interoperability Tests"
echo "============================"
echo ""

# Check if Go binaries exist
if [ ! -f "bin/go-server" ] || [ ! -f "bin/go-client" ]; then
    echo "Go binaries not found. Running setup..."
    ./setup.sh
    echo ""
fi

# Check if SBCL is available
if ! command -v sbcl &> /dev/null; then
    echo "ERROR: SBCL not found. Please install SBCL."
    exit 1
fi

# Helper function to wait for server to start
wait_for_server() {
    local port=$1
    local max_attempts=30
    local attempt=0

    echo "Waiting for server on port $port..."
    while ! nc -z localhost $port 2>/dev/null; do
        attempt=$((attempt + 1))
        if [ $attempt -ge $max_attempts ]; then
            echo "ERROR: Server did not start after $max_attempts seconds"
            return 1
        fi
        sleep 1
    done
    echo "Server is ready!"
}

# Helper function to kill background jobs
cleanup() {
    jobs -p | xargs -r kill 2>/dev/null || true
    sleep 1
}

trap cleanup EXIT

#
# Test 1: Go Server + Go Client (baseline)
#
echo "Test 1: Go Server + Go Client (baseline)"
echo "-----------------------------------------"
echo "Starting Go server in background..."
./bin/go-server > /tmp/go-server-test1.log 2>&1 &
SERVER_PID=$!

if ! wait_for_server 50051; then
    cat /tmp/go-server-test1.log
    exit 1
fi

echo "Running Go client..."
if ./bin/go-client > /tmp/go-client-test1.log 2>&1; then
    echo "✓ Go client successfully called Go server"
    grep "Greeting:" /tmp/go-client-test1.log
else
    echo "✗ Go client failed"
    cat /tmp/go-client-test1.log
    kill $SERVER_PID 2>/dev/null || true
    exit 1
fi

echo "Stopping Go server..."
kill $SERVER_PID 2>/dev/null || true
wait $SERVER_PID 2>/dev/null || true
sleep 1

echo ""
echo "Test 1 PASSED ✓"
echo ""

#
# Test 2: Go Server + CL Client
#
echo "Test 2: Go Server + CL Client"
echo "------------------------------"
echo "Starting Go server in background..."
./bin/go-server > /tmp/go-server-test2.log 2>&1 &
SERVER_PID=$!

if ! wait_for_server 50051; then
    cat /tmp/go-server-test2.log
    exit 1
fi

echo "Running CL client..."
if sbcl --script cl-client/client.lisp --run > /tmp/cl-client-test2.log 2>&1; then
    echo "✓ CL client successfully called Go server"
    grep -i "success" /tmp/cl-client-test2.log || cat /tmp/cl-client-test2.log
else
    echo "✗ CL client failed"
    cat /tmp/cl-client-test2.log
    kill $SERVER_PID 2>/dev/null || true
    exit 1
fi

echo "Stopping Go server..."
kill $SERVER_PID 2>/dev/null || true
wait $SERVER_PID 2>/dev/null || true
sleep 1

echo ""
echo "Test 2 PASSED ✓"
echo ""

#
# Test 3: CL Server + Go Client
#
echo "Test 3: CL Server + Go Client"
echo "------------------------------"
echo "Starting CL server in background..."
sbcl --script cl-server/server.lisp --run > /tmp/cl-server-test3.log 2>&1 &
SERVER_PID=$!

if ! wait_for_server 50051; then
    cat /tmp/cl-server-test3.log
    exit 1
fi

echo "Running Go client..."
if timeout 5 ./bin/go-client > /tmp/go-client-test3.log 2>&1; then
    echo "✓ Go client successfully called CL server"
    grep "Greeting:" /tmp/go-client-test3.log
else
    echo "✗ Go client failed"
    cat /tmp/go-client-test3.log
    kill $SERVER_PID 2>/dev/null || true
    exit 1
fi

echo "Stopping CL server..."
kill $SERVER_PID 2>/dev/null || true
wait $SERVER_PID 2>/dev/null || true
sleep 1

echo ""
echo "Test 3 PASSED ✓"
echo ""

#
# Test 4: CL Server + CL Client
#
echo "Test 4: CL Server + CL Client"
echo "------------------------------"
echo "Starting CL server in background..."
sbcl --script cl-server/server.lisp --run > /tmp/cl-server-test4.log 2>&1 &
SERVER_PID=$!

if ! wait_for_server 50051; then
    cat /tmp/cl-server-test4.log
    exit 1
fi

echo "Running CL client..."
if sbcl --script cl-client/client.lisp --run > /tmp/cl-client-test4.log 2>&1; then
    echo "✓ CL client successfully called CL server"
    grep -i "success" /tmp/cl-client-test4.log || cat /tmp/cl-client-test4.log
else
    echo "✗ CL client failed"
    cat /tmp/cl-client-test4.log
    kill $SERVER_PID 2>/dev/null || true
    exit 1
fi

echo "Stopping CL server..."
kill $SERVER_PID 2>/dev/null || true
wait $SERVER_PID 2>/dev/null || true
sleep 1

echo ""
echo "Test 4 PASSED ✓"
echo ""

echo "============================"
echo "All tests PASSED! ✓✓✓✓"
echo ""
echo "Summary:"
echo "  - Go-to-Go:  ✓"
echo "  - Go-to-CL:  ✓"
echo "  - CL-to-Go:  ✓"
echo "  - CL-to-CL:  ✓"
echo ""
echo "Full bidirectional interop working!"
