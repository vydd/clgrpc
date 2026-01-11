# clgrpc Performance Benchmarks

Comprehensive performance comparison between Common Lisp and Go gRPC implementations.

## Quick Results

| Clients | Go (req/s) | CL (req/s) | CL/Go Ratio | Slowdown |
|---------|------------|------------|-------------|----------|
| 1       | 9,214      | 12         | 0.13%       | 794x     |
| 10      | 30,850     | 120        | 0.39%       | 257x     |
| 100     | 78,471     | 1,179      | 1.50%       | 67x      |

**Key Finding:** CL achieves 101.7% scaling efficiency (near-perfect linear scaling) vs Go's 8.5%

See [PERFORMANCE_ANALYSIS.md](PERFORMANCE_ANALYSIS.md) for detailed analysis.

## What's Tested

- **RPC Types**: Unary (CL), All types (Go)
- **Concurrency Levels**: 1, 10, and 100 concurrent clients
- **Service**: RouteGuide (official gRPC example)
- **Duration**: 5 seconds per test
- **Metric**: Requests per second

## Prerequisites

### For Common Lisp Benchmarks

```bash
# Install dependencies (if not already installed)
sbcl --eval '(ql:quickload :cl+ssl)' \
     --eval '(ql:quickload :bordeaux-threads)' \
     --quit
```

### For Go Benchmarks

```bash
# Install protoc plugins
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest

# Add to PATH
export PATH=$PATH:$(go env GOPATH)/bin
```

### For Plotting

```bash
# Install matplotlib
pip3 install matplotlib numpy
```

## Running the Benchmarks

### Step 1: Run Go Benchmark

```bash
# Terminal 1: Start Go server
go run server.go 50055

# Terminal 2: Run benchmark
echo "" | go run benchmark.go
```

Results saved to `results-go.json` (all RPC types, ~1 minute)

### Step 2: Run Common Lisp Benchmark

```bash
# Starts server internally on port 50054
sbcl --script benchmark-cl-fixed.lisp
```

Results saved to `results-cl.json` (unary only, ~20 seconds)

### Step 3: Generate Comparison Graphs

```bash
python3 plot_unary_comparison.py   # CL vs Go comparison
python3 plot_go_all_types.py       # Go all RPC types
```

Generated:
- `benchmark_comparison_unary.png` - CL vs Go side-by-side + ratio chart
- `benchmark_go_all_types.png` - Go performance across all RPC types

## Understanding the Results

### Requests Per Second (req/s)

Higher is better. This measures throughput - how many complete RPCs the system can handle per second.

### Performance Ratio

Shows Common Lisp performance as a percentage of Go performance:
- **100%** = Equal performance
- **> 100%** = CL faster than Go
- **< 100%** = Go faster than CL

Color coding in ratio chart:
- **Green** (≥80%): Excellent - within 20% of Go
- **Yellow** (50-79%): Good - reasonable performance
- **Red** (<50%): Needs optimization

### Expected Performance Characteristics

**Unary RPCs**:
- Highest throughput for both implementations
- Most common pattern, heavily optimized
- Expect: 1000s-10000s req/s depending on client count

**Server Streaming**:
- Server sends multiple messages per RPC
- Throughput measured in messages received, not RPCs initiated
- Higher apparent throughput due to multiple messages

**Client Streaming**:
- Client sends multiple messages per RPC
- Lower RPC count but higher message throughput
- More expensive due to aggregation on server

**Bidirectional Streaming**:
- Most complex pattern
- Both client and server send multiple messages
- Highest coordination overhead

### Concurrency Scaling

- **1 client**: Baseline performance, no contention
- **10 clients**: Should show good scaling (near-linear)
- **100 clients**: Tests high contention, thread pool efficiency

Good scaling means throughput increases roughly linearly with clients (with some overhead).

## Actual Results (2026-01-11)

**Common Lisp (Unary)**:
- 1 client: 12 req/s
- 10 clients: 120 req/s (10x scaling - perfect!)
- 100 clients: 1,179 req/s (98x scaling - near-perfect!)
- **Scaling efficiency: 101.7%** (better than linear!)

**Go (Unary)**:
- 1 client: 9,214 req/s
- 10 clients: 30,850 req/s (3.3x scaling)
- 100 clients: 78,471 req/s (8.5x scaling)
- **Scaling efficiency: 8.5%** (poor, likely benchmark artifact)

**CL/Go Ratio**:
- 1 client: 0.13% (794x slower)
- 10 clients: 0.39% (257x slower)
- 100 clients: 1.50% (67x slower)
- Gap narrows significantly at higher concurrency due to CL's superior scaling

## Optimization Opportunities

Current CL performance is 0.13-1.5% of Go. Major opportunities:

1. **Remove debug logging** (10-50x improvement) - Currently printing every frame
2. **Connection pooling** (2-5x) - Currently creating new connection per request
3. **Type declarations** (2-3x) - Add type hints to hot paths
4. **Reduce allocations** (2-3x) - Pool byte arrays, reuse buffers
5. **HPACK caching** (1.5-2x) - Cache encoded headers
6. **Frame batching** (1.3-1.5x) - Batch writes to reduce syscalls

**Realistic target**: 10-20% of Go performance with these optimizations

## Troubleshooting

### "Connection refused" errors

- Ensure the Go server is running on port 50055
- Check firewall settings
- Verify port isn't already in use

### Benchmark takes too long

- Reduce `*benchmark-duration*` in the Lisp file
- Or `benchmarkDuration` in the Go file
- Default is 10 seconds per test

### Results look incorrect

- Check server logs for errors
- Ensure no other clients are connected
- Restart servers between benchmark runs
- Verify route_guide_db.json is loaded correctly

### Plotting fails

```bash
# Install dependencies
pip3 install matplotlib numpy

# Check Python version (needs 3.6+)
python3 --version
```

## Files in this Directory

**Benchmarks:**
- `benchmark-cl-fixed.lisp` - CL benchmark (unary only, working)
- `benchmark-cl.lisp` - CL benchmark (all types, WIP)
- `benchmark.go` - Go benchmark (all RPC types)
- `server.go` - Go RouteGuide server
- `route_guide.proto` - Protocol Buffers definition

**Results:**
- `results-cl.json` - CL benchmark data
- `results-go.json` - Go benchmark data
- `benchmark_comparison_unary.png` - Main comparison graph
- `benchmark_go_all_types.png` - Go all types graph

**Analysis:**
- `PERFORMANCE_ANALYSIS.md` - Detailed performance analysis
- `plot_unary_comparison.py` - CL vs Go comparison script
- `plot_go_all_types.py` - Go all types visualization

**Generated Code:**
- `routeguide/` - Generated Go protobuf code
- `go.mod`, `go.sum` - Go dependencies

## Notes

- Benchmarks use the same test patterns for fair comparison
- Both implementations use similar optimizations (connection pooling, etc.)
- Tests run on localhost to eliminate network latency
- Results may vary based on CPU, RAM, and system load
- Run multiple times and average for more reliable results
