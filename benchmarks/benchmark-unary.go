// benchmark-unary.go - Simple unary RPC benchmark

package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"sync"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	pb "clgrpc-benchmark/routeguide"
)

const (
	benchmarkPort     = "localhost:50055"
	benchmarkDuration = 5 * time.Second
)

type BenchmarkResult struct {
	RPCType           string  `json:"rpc_type"`
	NumClients        int     `json:"num_clients"`
	RequestsPerSecond float64 `json:"requests_per_second"`
	TotalRequests     int64   `json:"total_requests"`
	Errors            int64   `json:"errors"`
}

type workerResult struct {
	requests int64
	errors   int64
}

func benchmarkUnaryWorker(clientID int, duration time.Duration, results chan<- workerResult) {
	conn, err := grpc.Dial(benchmarkPort, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Printf("Client %d: Failed to connect: %v", clientID, err)
		results <- workerResult{0, 1}
		return
	}
	defer conn.Close()

	client := pb.NewRouteGuideClient(conn)
	ctx := context.Background()
	point := &pb.Point{Latitude: 409146138, Longitude: -746188906}

	var requests, errors int64
	startTime := time.Now()

	for time.Since(startTime) < duration {
		_, err := client.GetFeature(ctx, point)
		if err != nil {
			errors++
		} else {
			requests++
		}
	}

	results <- workerResult{requests, errors}
}

func runBenchmark(numClients int) BenchmarkResult {
	fmt.Printf("\nRunning unary benchmark with %d client(s) for %v...\n", numClients, benchmarkDuration)

	results := make(chan workerResult, numClients)
	var wg sync.WaitGroup

	startTime := time.Now()

	for clientID := 1; clientID <= numClients; clientID++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			benchmarkUnaryWorker(id, benchmarkDuration, results)
		}(clientID)
	}

	wg.Wait()
	close(results)

	duration := time.Since(startTime).Seconds()

	var totalRequests, totalErrors int64
	for result := range results {
		totalRequests += result.requests
		totalErrors += result.errors
	}

	reqPerSec := float64(totalRequests) / duration

	fmt.Printf("  Total Requests: %d\n", totalRequests)
	fmt.Printf("  Total Errors: %d\n", totalErrors)
	fmt.Printf("  Requests/sec: %.1f\n", reqPerSec)

	return BenchmarkResult{
		RPCType:           "unary",
		NumClients:        numClients,
		RequestsPerSecond: reqPerSec,
		TotalRequests:     totalRequests,
		Errors:            totalErrors,
	}
}

func main() {
	fmt.Println("\n╔════════════════════════════════════════════════════╗")
	fmt.Println("║  Unary RPC Performance Benchmark                   ║")
	fmt.Println("╚════════════════════════════════════════════════════╝")

	fmt.Println("\nMake sure the server is running on port 50055")

	var allResults []BenchmarkResult

	clientCounts := []int{1, 10, 100}

	for _, numClients := range clientCounts {
		result := runBenchmark(numClients)
		allResults = append(allResults, result)
	}

	// Print summary
	fmt.Println("\n╔════════════════════════════════════════════════════╗")
	fmt.Println("║  Benchmark Results Summary                         ║")
	fmt.Println("╚════════════════════════════════════════════════════╝\n")

	fmt.Printf("%-12s %-15s %-15s %-15s\n", "Clients", "Req/Sec", "Total Reqs", "Errors")
	fmt.Println("------------------------------------------------------------")

	for _, result := range allResults {
		fmt.Printf("%-12d %-15.1f %-15d %-15d\n",
			result.NumClients,
			result.RequestsPerSecond,
			result.TotalRequests,
			result.Errors)
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
