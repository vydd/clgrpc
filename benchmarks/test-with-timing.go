// test-with-timing.go - Detailed timing breakdown

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
	fmt.Println("Connecting to server...")
	connectStart := time.Now()

	conn, err := grpc.Dial("localhost:50055", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("Failed to connect: %v", err)
	}
	defer conn.Close()

	connectDuration := time.Since(connectStart)
	fmt.Printf("Connection established in: %v\n\n", connectDuration)

	client := pb.NewRouteGuideClient(conn)
	ctx := context.Background()
	point := &pb.Point{Latitude: 409146138, Longitude: -746188906}

	fmt.Println("Sending 10 requests with detailed timing...")
	for i := 1; i <= 10; i++ {
		start := time.Now()
		_, err := client.GetFeature(ctx, point)
		duration := time.Since(start)

		if err != nil {
			fmt.Printf("Request %2d: FAILED in %v\n", i, duration)
		} else {
			fmt.Printf("Request %2d: %v\n", i, duration)
		}
	}

	fmt.Println("\nClosing connection...")
}
