package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	pb "clgrpc-benchmark/routeguide"
)

func main() {
	conn, err := grpc.Dial("localhost:50055", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("Failed to connect: %v", err)
	}
	defer conn.Close()

	client := pb.NewRouteGuideClient(conn)
	ctx := context.Background()
	point := &pb.Point{Latitude: 409146138, Longitude: -746188906}

	fmt.Println("Running 5 second benchmark with 1 client...")
	
	startTime := time.Now()
	var requests, errors int64
	
	for time.Since(startTime) < 5*time.Second {
		_, err := client.GetFeature(ctx, point)
		if err != nil {
			errors++
		} else {
			requests++
		}
	}
	
	duration := time.Since(startTime).Seconds()
	rps := float64(requests) / duration
	
	result := map[string]interface{}{
		"requests_per_second": rps,
		"total_requests":      requests,
		"errors":              errors,
	}
	
	data, _ := json.MarshalIndent(result, "", "  ")
	os.WriteFile("results-1-client.json", data, 0644)
	
	fmt.Printf("\nResults:\n")
	fmt.Printf("  Total Requests: %d\n", requests)
	fmt.Printf("  Errors: %d\n", errors)
	fmt.Printf("  Requests/sec: %.1f\n", rps)
}
