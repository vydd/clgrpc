// test-two-requests.go - Send exactly 2 requests to test server

package main

import (
	"context"
	"fmt"
	"log"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	pb "clgrpc-benchmark/routeguide"
)

func main() {
	fmt.Println("Connecting to server on localhost:50055...")

	conn, err := grpc.Dial("localhost:50055", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("Failed to connect: %v", err)
	}
	defer conn.Close()

	client := pb.NewRouteGuideClient(conn)
	ctx := context.Background()

	point := &pb.Point{Latitude: 409146138, Longitude: -746188906}

	fmt.Println("\n=== Sending Request 1 ===")
	start := time.Now()
	feature, err := client.GetFeature(ctx, point)
	duration := time.Since(start)

	if err != nil {
		fmt.Printf("❌ Request 1 FAILED after %v: %v\n", duration, err)
	} else {
		fmt.Printf("✓ Request 1 SUCCESS in %v\n", duration)
		fmt.Printf("  Response: %v\n", feature.Name)
	}

	// Wait a bit
	time.Sleep(100 * time.Millisecond)

	fmt.Println("\n=== Sending Request 2 ===")
	start = time.Now()
	feature, err = client.GetFeature(ctx, point)
	duration = time.Since(start)

	if err != nil {
		fmt.Printf("❌ Request 2 FAILED after %v: %v\n", duration, err)
	} else {
		fmt.Printf("✓ Request 2 SUCCESS in %v\n", duration)
		fmt.Printf("  Response: %v\n", feature.Name)
	}

	fmt.Println("\nDone!")
}
