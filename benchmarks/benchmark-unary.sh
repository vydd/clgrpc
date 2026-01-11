#!/bin/bash
# Benchmark unary RPC only: Go Client vs CL/Go Servers

set -e

echo "═══════════════════════════════════════════════════════════"
echo "  Unary RPC Server Benchmark"
echo "  Go Client → CL Server vs Go Client → Go Server"
echo "═══════════════════════════════════════════════════════════"
echo ""

cd /home/vydd/Code/clgrpc/benchmarks

# Clean up any existing processes
pkill -9 sbcl 2>/dev/null || true
pkill -9 server 2>/dev/null || true
sleep 2

# Build benchmark tools
echo "Building Go benchmark tools..."
go build -o bench-unary benchmark-unary.go
go build -o server server.go
echo ""

#================================================================
# Test 1: Go Client → CL Server
#================================================================
echo "─────────────────────────────────────────────────────────────"
echo "Test 1: Go Client → CL Server (port 50055)"
echo "─────────────────────────────────────────────────────────────"

# Start CL server
echo "Starting CL server..."
cat > /tmp/start-cl-server.lisp <<'LISP'
(ql:quickload :clgrpc-examples :silent t)
(in-package :clgrpc-examples)

;; Load route guide database
(defparameter *route-features-clos*
  (routeguide-clos-load-features
   #P"/home/vydd/Code/clgrpc/examples/routeguide/route_guide_db.json"))

;; Create and start server on port 50055
(let ((server (make-server :port 50055)))
  (let ((route-guide (make-instance 'route-guide-service-clos)))
    (register-service (grpc-server-router server) route-guide))
  (format t "CL server starting on port 50055...~%")
  (start-server server)
  (format t "CL server ready!~%")
  (loop (sleep 1)))
LISP

sbcl --noinform --load /tmp/start-cl-server.lisp > /tmp/cl-server.log 2>&1 &
CL_PID=$!

# Wait for server to be ready
echo "Waiting for CL server..."
for i in {1..30}; do
    if lsof -i :50055 2>/dev/null | grep -q LISTEN; then
        echo "✓ CL server is ready"
        break
    fi
    sleep 1
    if [ $i -eq 30 ]; then
        echo "✗ CL server failed to start!"
        tail -50 /tmp/cl-server.log
        kill $CL_PID 2>/dev/null || true
        exit 1
    fi
done
sleep 2

# Run Go client against CL server
echo "Running Go client benchmark..."
./bench-unary > /dev/null 2>&1
mv results-go.json results-cl-server.json
echo ""
echo "CL Server Results:"
cat results-cl-server.json | python3 -m json.tool
echo ""

# Stop CL server
kill $CL_PID 2>/dev/null || true
sleep 3

#================================================================
# Test 2: Go Client → Go Server
#================================================================
echo "─────────────────────────────────────────────────────────────"
echo "Test 2: Go Client → Go Server (port 50055)"
echo "─────────────────────────────────────────────────────────────"

# Start Go server
echo "Starting Go server..."
./server > /tmp/go-server.log 2>&1 &
GO_PID=$!

# Wait for server
echo "Waiting for Go server..."
for i in {1..20}; do
    if lsof -i :50055 2>/dev/null | grep -q LISTEN; then
        echo "✓ Go server is ready"
        break
    fi
    sleep 1
    if [ $i -eq 20 ]; then
        echo "✗ Go server failed to start!"
        tail -50 /tmp/go-server.log
        kill $GO_PID 2>/dev/null || true
        exit 1
    fi
done
sleep 2

# Run Go client against Go server
echo "Running Go client benchmark..."
./bench-unary > /dev/null 2>&1
mv results-go.json results-go-server.json
echo ""
echo "Go Server Results:"
cat results-go-server.json | python3 -m json.tool
echo ""

# Stop Go server
kill $GO_PID 2>/dev/null || true

#================================================================
# Summary
#================================================================
echo "═══════════════════════════════════════════════════════════"
echo "  Performance Comparison Summary"
echo "═══════════════════════════════════════════════════════════"
echo ""

echo "CL Server:"
jq -r '.[] | "\(.num_clients) clients: \(.requests_per_second | floor) req/sec, \(.errors) errors"' results-cl-server.json

echo ""
echo "Go Server:"
jq -r '.[] | "\(.num_clients) clients: \(.requests_per_second | floor) req/sec, \(.errors) errors"' results-go-server.json

echo ""
echo "Done!"
