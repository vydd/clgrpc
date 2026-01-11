// test-concurrency.go - Test concurrent clients

package main

import (
	"context"
	"fmt"
	"log"
	"sync"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	pb "clgrpc-benchmark/routeguide"
)

func worker(clientID int, duration time.Duration, results chan<- struct{ requests, errors int }) {
	conn, err := grpc.Dial("localhost:50055", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Printf("Client %d: Failed to connect: %v", clientID, err)
		results <- struct{ requests, errors int }{0, 1}
		return
	}
	defer conn.Close()

	client := pb.NewRouteGuideClient(conn)
	point := &pb.Point{Latitude: 409146138, Longitude: -746188906}

	var requests, errors int
	startTime := time.Now()

	for time.Since(startTime) < duration {
		ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
		_, err := client.GetFeature(ctx, point)
		cancel()

		if err != nil {
			errors++
		} else {
			requests++
		}
	}

	results <- struct{ requests, errors int }{requests, errors}
}

func runTest(numClients int, duration time.Duration) {
	fmt.Printf("\n═══════════════════════════════════════════════════════════\n")
	fmt.Printf("Testing with %d concurrent clients for %v\n", numClients, duration)
	fmt.Printf("═══════════════════════════════════════════════════════════\n")

	results := make(chan struct{ requests, errors int }, numClients)
	var wg sync.WaitGroup

	startTime := time.Now()

	for i := 1; i <= numClients; i++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			worker(id, duration, results)
		}(i)
	}

	wg.Wait()
	close(results)

	elapsed := time.Since(startTime).Seconds()

	var totalRequests, totalErrors int
	for result := range results {
		totalRequests += result.requests
		totalErrors += result.errors
	}

	reqPerSec := float64(totalRequests) / elapsed

	fmt.Printf("\nResults:\n")
	fmt.Printf("  Duration:       %.2fs\n", elapsed)
	fmt.Printf("  Requests:       %d\n", totalRequests)
	fmt.Printf("  Errors:         %d\n", totalErrors)
	fmt.Printf("  Requests/sec:   %.1f\n", reqPerSec)
	fmt.Printf("  Success rate:   %.1f%%\n", 100.0*float64(totalRequests)/float64(totalRequests+totalErrors))
}

func main() {
	fmt.Println("Common Lisp gRPC Server Concurrency Test")
	fmt.Println("Make sure server is running on port 50055")

	testDuration := 5 * time.Second

	runTest(1, testDuration)
	runTest(10, testDuration)
	runTest(100, testDuration)

	fmt.Println("\n✓ All tests completed!")
}
