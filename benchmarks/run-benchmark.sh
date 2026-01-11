#!/bin/bash
# run-benchmark.sh - Run both CL and Go benchmarks

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "╔════════════════════════════════════════════════════╗"
echo "║  gRPC Performance Benchmark Suite                  ║"
echo "╚════════════════════════════════════════════════════╝"
echo ""

# Check if we should run CL benchmark
if [ "$1" != "--go-only" ]; then
    echo "Step 1: Running Common Lisp benchmark..."
    echo "This will take approximately 2-3 minutes."
    echo ""

    sbcl --script benchmark-cl.lisp

    if [ ! -f "results-cl.json" ]; then
        echo "Error: CL benchmark did not produce results-cl.json"
        exit 1
    fi

    echo ""
    echo "✓ Common Lisp benchmark complete"
    echo ""
fi

# Check if Go benchmarks should be run
if [ "$1" == "--cl-only" ]; then
    echo "Skipping Go benchmark (--cl-only specified)"
else
    echo "Step 2: Running Go benchmark..."
    echo ""
    echo "IMPORTANT: You must start the Go RouteGuide server first!"
    echo "In another terminal, run:"
    echo "  cd \$GOPATH/src/google.golang.org/grpc/examples/route_guide"
    echo "  go run server/server.go -port 50055"
    echo ""
    echo "Press Enter when the server is ready, or Ctrl+C to skip..."
    read

    go run benchmark-go.go

    if [ ! -f "results-go.json" ]; then
        echo "Error: Go benchmark did not produce results-go.json"
        exit 1
    fi

    echo ""
    echo "✓ Go benchmark complete"
    echo ""
fi

# Generate graphs
if [ -f "results-cl.json" ] && [ -f "results-go.json" ]; then
    echo "Step 3: Generating performance graphs..."
    echo ""

    python3 plot_results.py

    echo ""
    echo "╔════════════════════════════════════════════════════╗"
    echo "║  Benchmark Complete!                               ║"
    echo "╚════════════════════════════════════════════════════╝"
    echo ""
    echo "Results:"
    echo "  • results-cl.json           - CL benchmark data"
    echo "  • results-go.json           - Go benchmark data"
    echo "  • performance_comparison.png - Comparison by RPC type"
    echo "  • performance_overall.png    - Overall comparison"
    echo "  • performance_ratio.png      - CL vs Go performance ratio"
    echo ""
elif [ -f "results-cl.json" ]; then
    echo "Only CL results available. Run Go benchmark to create comparison graphs."
else
    echo "No results available."
fi
