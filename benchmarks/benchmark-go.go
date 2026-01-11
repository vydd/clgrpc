// benchmark-go.go - Performance benchmarks for Go gRPC
//
// Measures requests per second across different RPC types and client counts

package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"sync"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	pb "google.golang.org/grpc/examples/route_guide/routeguide"
)

const (
	benchmarkPort     = "localhost:50055"
	benchmarkDuration = 10 * time.Second
)

type BenchmarkResult struct {
	RPCType            string  `json:"rpc_type"`
	NumClients         int     `json:"num_clients"`
	RequestsPerSecond  float64 `json:"requests_per_second"`
	TotalRequests      int64   `json:"total_requests"`
	Errors             int64   `json:"errors"`
}

type workerResult struct {
	requests int64
	errors   int64
}

// Unary RPC benchmark worker
func benchmarkUnaryWorker(clientID int, duration time.Duration, results chan<- workerResult) {
	conn, err := grpc.Dial(benchmarkPort, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		results <- workerResult{0, 1}
		return
	}
	defer conn.Close()

	client := pb.NewRouteGuideClient(conn)
	ctx := context.Background()

	var requestCount int64
	var errorCount int64
	startTime := time.Now()

	for time.Since(startTime) < duration {
		point := &pb.Point{Latitude: 409146138, Longitude: -746188906}
		_, err := client.GetFeature(ctx, point)
		if err != nil {
			errorCount++
		} else {
			requestCount++
		}
	}

	results <- workerResult{requestCount, errorCount}
}

// Server streaming RPC benchmark worker
func benchmarkServerStreamingWorker(clientID int, duration time.Duration, results chan<- workerResult) {
	conn, err := grpc.Dial(benchmarkPort, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		results <- workerResult{0, 1}
		return
	}
	defer conn.Close()

	client := pb.NewRouteGuideClient(conn)
	ctx := context.Background()

	var requestCount int64
	var errorCount int64
	startTime := time.Now()

	for time.Since(startTime) < duration {
		rect := &pb.Rectangle{
			Lo: &pb.Point{Latitude: 400000000, Longitude: -750000000},
			Hi: &pb.Point{Latitude: 420000000, Longitude: -730000000},
		}

		stream, err := client.ListFeatures(ctx, rect)
		if err != nil {
			errorCount++
			continue
		}

		for {
			_, err := stream.Recv()
			if err == io.EOF {
				break
			}
			if err != nil {
				errorCount++
				break
			}
			requestCount++
		}
	}

	results <- workerResult{requestCount, errorCount}
}

// Client streaming RPC benchmark worker
func benchmarkClientStreamingWorker(clientID int, duration time.Duration, results chan<- workerResult) {
	conn, err := grpc.Dial(benchmarkPort, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		results <- workerResult{0, 1}
		return
	}
	defer conn.Close()

	client := pb.NewRouteGuideClient(conn)
	ctx := context.Background()

	var requestCount int64
	var errorCount int64
	startTime := time.Now()

	for time.Since(startTime) < duration {
		stream, err := client.RecordRoute(ctx)
		if err != nil {
			errorCount++
			continue
		}

		// Send 10 points
		for i := 0; i < 10; i++ {
			point := &pb.Point{
				Latitude:  int32(400000000 + i*1000000),
				Longitude: int32(-740000000 - i*1000000),
			}
			if err := stream.Send(point); err != nil {
				errorCount++
				break
			}
		}

		_, err = stream.CloseAndRecv()
		if err != nil {
			errorCount++
		} else {
			requestCount++
		}
	}

	results <- workerResult{requestCount, errorCount}
}

// Bidirectional streaming RPC benchmark worker
func benchmarkBidiStreamingWorker(clientID int, duration time.Duration, results chan<- workerResult) {
	conn, err := grpc.Dial(benchmarkPort, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		results <- workerResult{0, 1}
		return
	}
	defer conn.Close()

	client := pb.NewRouteGuideClient(conn)
	ctx := context.Background()

	var requestCount int64
	var errorCount int64
	startTime := time.Now()

	for time.Since(startTime) < duration {
		stream, err := client.RouteChat(ctx)
		if err != nil {
			errorCount++
			continue
		}

		// Send 5 notes
		for i := 0; i < 5; i++ {
			note := &pb.RouteNote{
				Location: &pb.Point{
					Latitude:  int32(400000000 + i*1000000),
					Longitude: int32(-740000000 - i*1000000),
				},
				Message: fmt.Sprintf("Message %d from client %d", i, clientID),
			}
			if err := stream.Send(note); err != nil {
				errorCount++
				break
			}
		}

		if err := stream.CloseSend(); err != nil {
			errorCount++
			continue
		}

		// Read all responses
		for {
			_, err := stream.Recv()
			if err == io.EOF {
				break
			}
			if err != nil {
				errorCount++
				break
			}
			requestCount++
		}
	}

	results <- workerResult{requestCount, errorCount}
}

// Run benchmark for a specific RPC type and client count
func runBenchmark(rpcType string, numClients int, workerFn func(int, time.Duration, chan<- workerResult)) BenchmarkResult {
	fmt.Printf("\nRunning %s benchmark with %d client(s) for %v...\n", rpcType, numClients, benchmarkDuration)

	results := make(chan workerResult, numClients)
	var wg sync.WaitGroup

	startTime := time.Now()

	// Start workers
	for i := 0; i < numClients; i++ {
		wg.Add(1)
		go func(clientID int) {
			defer wg.Done()
			workerFn(clientID, benchmarkDuration, results)
		}(i)
	}

	wg.Wait()
	close(results)

	duration := time.Since(startTime).Seconds()

	// Aggregate results
	var totalRequests int64
	var totalErrors int64
	for result := range results {
		totalRequests += result.requests
		totalErrors += result.errors
	}

	reqPerSec := float64(totalRequests) / duration

	fmt.Printf("  Total requests: %d\n", totalRequests)
	fmt.Printf("  Total errors: %d\n", totalErrors)
	fmt.Printf("  Requests/sec: %.1f\n\n", reqPerSec)

	return BenchmarkResult{
		RPCType:            rpcType,
		NumClients:         numClients,
		RequestsPerSecond:  reqPerSec,
		TotalRequests:      totalRequests,
		Errors:             totalErrors,
	}
}

func main() {
	fmt.Println("\n╔════════════════════════════════════════════════════╗")
	fmt.Println("║  Go gRPC Performance Benchmark                     ║")
	fmt.Println("╚════════════════════════════════════════════════════╝")

	fmt.Println("\nNote: Start the Go RouteGuide server on port 50055 before running.")
	fmt.Println("Press Enter to continue...")
	fmt.Scanln()

	var allResults []BenchmarkResult

	// Run benchmarks for each RPC type and client count
	rpcTypes := []struct {
		name     string
		workerFn func(int, time.Duration, chan<- workerResult)
	}{
		{"unary", benchmarkUnaryWorker},
		{"server-streaming", benchmarkServerStreamingWorker},
		{"client-streaming", benchmarkClientStreamingWorker},
		{"bidi-streaming", benchmarkBidiStreamingWorker},
	}

	clientCounts := []int{1, 10, 100}

	for _, rpcType := range rpcTypes {
		for _, numClients := range clientCounts {
			result := runBenchmark(rpcType.name, numClients, rpcType.workerFn)
			allResults = append(allResults, result)
		}
	}

	// Print summary
	fmt.Println("\n╔════════════════════════════════════════════════════╗")
	fmt.Println("║  Benchmark Results Summary                         ║")
	fmt.Println("╚════════════════════════════════════════════════════╝\n")

	fmt.Printf("%-25s %-12s %-15s\n", "RPC Type", "Clients", "Req/Sec")
	fmt.Println("------------------------------------------------------------")

	for _, result := range allResults {
		fmt.Printf("%-25s %-12d %-15.1f\n",
			result.RPCType,
			result.NumClients,
			result.RequestsPerSecond)
	}

	// Save results to JSON file
	jsonData, err := json.MarshalIndent(allResults, "", "  ")
	if err != nil {
		log.Fatalf("Failed to marshal results: %v", err)
	}

	err = os.WriteFile("results-go.json", jsonData, 0644)
	if err != nil {
		log.Fatalf("Failed to write results: %v", err)
	}

	fmt.Println("\nResults saved to results-go.json")
}
