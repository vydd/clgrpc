#!/bin/bash
# Benchmark server performance by using Go client against both CL and Go servers

set -e

echo "═══════════════════════════════════════════════════════════"
echo "  Server Performance Benchmark: Go Client vs Both Servers"
echo "═══════════════════════════════════════════════════════════"
echo ""
echo "This benchmark isolates server performance by using the same"
echo "Go client against both the CL server and Go server."
echo ""

# Clean up any existing servers
pkill -9 sbcl 2>/dev/null || true
pkill -9 server 2>/dev/null || true
sleep 2

# Build Go client benchmark (modify port via environment variable)
echo "Building Go client benchmark..."
cd /home/vydd/Code/clgrpc/benchmarks
go build -o bench-client benchmark.go

# Build Go server
echo "Building Go server..."
go build -o server server.go

echo ""
echo "─────────────────────────────────────────────────────────────"
echo "Test 1: Go Client → CL Server"
echo "─────────────────────────────────────────────────────────────"
echo ""

# Start CL server on port 50054
echo "Starting CL server on port 50054..."
sbcl --noinform --eval '(ql:quickload :clgrpc-examples :silent t)' \
     --eval '(in-package :clgrpc-examples)' \
     --eval '(start-routeguide-server 50054)' \
     --eval '(loop (sleep 1))' \
     > /tmp/cl-server.log 2>&1 &
CL_PID=$!

# Wait for server to be ready
echo "Waiting for CL server to start..."
for i in {1..30}; do
    if lsof -i :50054 2>/dev/null | grep -q LISTEN; then
        echo "CL server ready!"
        break
    fi
    sleep 1
    if [ $i -eq 30 ]; then
        echo "ERROR: CL server failed to start"
        cat /tmp/cl-server.log
        kill $CL_PID 2>/dev/null || true
        exit 1
    fi
done

# Modify Go client to use port 50054
echo "Running Go client against CL server (10 clients, 5 seconds)..."
sed -i 's/localhost:50055/localhost:50054/g' benchmark.go
go build -o bench-client benchmark.go

./bench-client > results-cl-server.json
cat results-cl-server.json

# Stop CL server
echo ""
echo "Stopping CL server..."
kill $CL_PID 2>/dev/null || true
sleep 2

echo ""
echo "─────────────────────────────────────────────────────────────"
echo "Test 2: Go Client → Go Server"
echo "─────────────────────────────────────────────────────────────"
echo ""

# Restore Go client to use port 50055
sed -i 's/localhost:50054/localhost:50055/g' benchmark.go
go build -o bench-client benchmark.go

# Start Go server on port 50055
echo "Starting Go server on port 50055..."
./server > /tmp/go-server.log 2>&1 &
GO_PID=$!

# Wait for server to be ready
echo "Waiting for Go server to start..."
for i in {1..10}; do
    if lsof -i :50055 2>/dev/null | grep -q LISTEN; then
        echo "Go server ready!"
        break
    fi
    sleep 1
done

echo "Running Go client against Go server (10 clients, 5 seconds)..."
./bench-client > results-go-server.json
cat results-go-server.json

# Stop Go server
echo ""
echo "Stopping Go server..."
kill $GO_PID 2>/dev/null || true

echo ""
echo "═══════════════════════════════════════════════════════════"
echo "  Results Comparison"
echo "═══════════════════════════════════════════════════════════"
echo ""

# Parse and compare results
python3 <<'EOF'
import json

with open('results-cl-server.json') as f:
    cl_results = json.load(f)

with open('results-go-server.json') as f:
    go_results = json.load(f)

print("Server Performance Comparison (Go Client):")
print("=" * 70)
print(f"{'Metric':<30} {'CL Server':>15} {'Go Server':>15} {'Ratio':>8}")
print("-" * 70)

for cl_row, go_row in zip(cl_results, go_results):
    rpc_type = cl_row['rpc_type']
    clients = cl_row['num_clients']

    cl_rps = cl_row['requests_per_second']
    go_rps = go_row['requests_per_second']

    ratio = go_rps / cl_rps if cl_rps > 0 else 0

    print(f"{rpc_type} ({clients} clients):".<30} {cl_rps:>12,.1f} rps {go_rps:>12,.1f} rps {ratio:>7.1f}x")

print("=" * 70)
print("")
print("Interpretation:")
print("- Ratio > 1: Go server is faster")
print("- Ratio < 1: CL server is faster")
print("- This isolates SERVER performance (client is the same)")
EOF

echo ""
echo "Results saved to:"
echo "  - results-cl-server.json (CL server)"
echo "  - results-go-server.json (Go server)"
