// test-10-requests.go - Send exactly 10 sequential requests

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

	fmt.Println("\nSending 10 sequential requests...")
	overallStart := time.Now()

	for i := 1; i <= 10; i++ {
		start := time.Now()
		feature, err := client.GetFeature(ctx, point)
		duration := time.Since(start)

		if err != nil {
			fmt.Printf("Request %2d: FAILED after %v: %v\n", i, duration, err)
		} else {
			fmt.Printf("Request %2d: SUCCESS in %v\n", i, duration)
			if i == 1 {
				fmt.Printf("            Response: %v\n", feature.Name)
			}
		}
	}

	overallDuration := time.Since(overallStart)
	fmt.Printf("\nTotal time: %v\n", overallDuration)
	fmt.Printf("Average: %v per request\n", overallDuration/10)
	fmt.Printf("Rate: %.1f req/sec\n", 10.0/overallDuration.Seconds())
}
