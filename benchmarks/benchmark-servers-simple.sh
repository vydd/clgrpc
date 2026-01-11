#!/bin/bash
# Simple server benchmark: Go client against CL server vs Go server

set -e

echo "═══════════════════════════════════════════════════════════"
echo "  Server Performance Benchmark"
echo "  Go Client → CL Server vs Go Client → Go Server"
echo "═══════════════════════════════════════════════════════════"
echo ""

# Clean up
pkill -9 sbcl 2>/dev/null || true
pkill -9 server 2>/dev/null || true
sleep 2

cd /home/vydd/Code/clgrpc/benchmarks

# Build Go tools
echo "Building Go benchmark tools..."
go build -o bench-client benchmark.go
go build -o server server.go

echo ""
echo "─────────────────────────────────────────────────────────────"
echo "Test 1: Go Client → CL Server (port 50055)"
echo "─────────────────────────────────────────────────────────────"

# Start CL server (modify the existing example to use port 50055)
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

# Wait for CL server
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
./bench-client > /dev/null 2>&1
mv results-go.json results-cl-server.json
echo ""
echo "CL Server Results:"
cat results-cl-server.json | python3 -m json.tool | head -30
echo ""

# Stop CL server
kill $CL_PID 2>/dev/null || true
sleep 3

echo "─────────────────────────────────────────────────────────────"
echo "Test 2: Go Client → Go Server (port 50055)"
echo "─────────────────────────────────────────────────────────────"

# Start Go server
echo "Starting Go server..."
./server > /tmp/go-server.log 2>&1 &
GO_PID=$!

# Wait for Go server
echo "Waiting for Go server..."
for i in {1..10}; do
    if lsof -i :50055 2>/dev/null | grep -q LISTEN; then
        echo "✓ Go server is ready"
        break
    fi
    sleep 1
done
sleep 1

# Run Go client against Go server
echo "Running Go client benchmark..."
./bench-client > /dev/null 2>&1
mv results-go.json results-go-server.json
echo ""
echo "Go Server Results:"
cat results-go-server.json | python3 -m json.tool | head -30
echo ""

# Stop Go server
kill $GO_PID 2>/dev/null || true

echo "═══════════════════════════════════════════════════════════"
echo "  Comparison"
echo "═══════════════════════════════════════════════════════════"
echo ""

# Compare results
python3 <<'EOF'
import json

try:
    with open('results-cl-server.json') as f:
        cl_results = json.load(f)
    with open('results-go-server.json') as f:
        go_results = json.load(f)

    print(f"{'Test Case':<35} {'CL Server':>15} {'Go Server':>15} {'Slowdown':>10}")
    print("=" * 78)

    for cl, go in zip(cl_results, go_results):
        test = f"{cl['rpc_type']} ({cl['num_clients']} clients)"
        cl_rps = cl['requests_per_second']
        go_rps = go['requests_per_second']
        ratio = go_rps / cl_rps if cl_rps > 0 else 0

        print(f"{test:<35} {cl_rps:>12,.1f} rps {go_rps:>12,.1f} rps {ratio:>9.1f}x")

    print("=" * 78)
    print("")
    print("Note: Slowdown shows how many times SLOWER the CL server is")
    print("      (Go Client performance is constant in both tests)")

except Exception as e:
    print(f"Error comparing results: {e}")
    import traceback
    traceback.print_exc()
EOF

echo ""
echo "Results saved to:"
echo "  - results-cl-server.json"
echo "  - results-go-server.json"
