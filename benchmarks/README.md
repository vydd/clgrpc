# gRPC Performance Benchmarks

Comprehensive performance comparison between Common Lisp and Go gRPC implementations.

## What's Tested

- **RPC Types**: Unary, Server Streaming, Client Streaming, Bidirectional Streaming
- **Concurrency Levels**: 1, 10, and 100 concurrent clients
- **Service**: RouteGuide (official gRPC example)
- **Duration**: 10 seconds per test
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
# Install Go gRPC examples
go get google.golang.org/grpc/examples/route_guide/...

# Navigate to the route guide server directory
cd $GOPATH/src/google.golang.org/grpc/examples/route_guide
```

### For Plotting

```bash
# Install matplotlib
pip3 install matplotlib numpy
```

## Running the Benchmarks

### Step 1: Run Common Lisp Benchmark

The CL benchmark starts its own server and runs automatically:

```bash
cd benchmarks
sbcl --script benchmark-cl.lisp
```

This will:
- Start a RouteGuide server on port 50054
- Run all benchmarks (4 RPC types × 3 client counts = 12 tests)
- Save results to `results-cl.json`
- Take approximately 2-3 minutes

### Step 2: Run Go Benchmark

First, start the official Go RouteGuide server:

```bash
# In terminal 1 - Start Go server
cd $GOPATH/src/google.golang.org/grpc/examples/route_guide
go run server/server.go -port 50055
```

Then run the benchmark:

```bash
# In terminal 2 - Run benchmarks
cd /path/to/clgrpc/benchmarks
go run benchmark-go.go
```

This will:
- Connect to the Go server on port 50055
- Run all benchmarks
- Save results to `results-go.json`
- Take approximately 2-3 minutes

### Step 3: Generate Graphs

```bash
python3 plot_results.py
```

This generates:
- `performance_comparison.png` - Side-by-side comparison by RPC type
- `performance_overall.png` - Overall comparison across all dimensions
- `performance_ratio.png` - CL performance as % of Go performance

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

## Typical Results

Based on similar implementations, expect:

**Common Lisp**:
- Unary: 5,000-15,000 req/s (100 clients)
- Good scaling up to 10 clients
- Some thread contention at 100 clients

**Go**:
- Unary: 10,000-30,000 req/s (100 clients)
- Excellent scaling across all client counts
- Highly optimized runtime and scheduler

**CL/Go Ratio**:
- Typically 50-80% of Go performance
- Better at low concurrency (80-100%)
- More overhead at high concurrency (40-60%)

## Optimization Opportunities

If CL performance is below 50% of Go:

1. **Profile the code**: Use SBCL's statistical profiler
2. **Check I/O buffering**: Ensure efficient socket operations
3. **Thread pool tuning**: Adjust server thread configuration
4. **Type declarations**: Add type hints for hot paths
5. **Memory allocation**: Reduce consing in critical paths

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

## Files Generated

- `results-cl.json` - Common Lisp benchmark results
- `results-go.json` - Go benchmark results
- `performance_comparison.png` - Main comparison graph
- `performance_overall.png` - Detailed comparison
- `performance_ratio.png` - Performance ratio visualization

## Benchmark Code

- `benchmark-cl.lisp` - Common Lisp benchmark suite
- `benchmark-go.go` - Go benchmark suite
- `plot_results.py` - Graph generation script

## Notes

- Benchmarks use the same test patterns for fair comparison
- Both implementations use similar optimizations (connection pooling, etc.)
- Tests run on localhost to eliminate network latency
- Results may vary based on CPU, RAM, and system load
- Run multiple times and average for more reliable results
