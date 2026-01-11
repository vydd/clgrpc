// test-continuous.go - Send continuous requests for 3 seconds

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
	conn, err := grpc.Dial("localhost:50055", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("Failed to connect: %v", err)
	}
	defer conn.Close()

	client := pb.NewRouteGuideClient(conn)
	point := &pb.Point{Latitude: 409146138, Longitude: -746188906}

	fmt.Println("Sending continuous requests for 3 seconds...")
	count := 0
	errors := 0
	startTime := time.Now()

	for time.Since(startTime) < 3*time.Second {
		ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
		_, err := client.GetFeature(ctx, point)
		cancel()

		if err != nil {
			errors++
			fmt.Printf("Request %d: ERROR - %v\n", count+1, err)
		} else {
			count++
		}

		if (count+errors)%100 == 0 {
			fmt.Printf("Progress: %d successful, %d errors\n", count, errors)
		}
	}

	duration := time.Since(startTime).Seconds()
	fmt.Printf("\nCompleted in %.2fs\n", duration)
	fmt.Printf("Successful: %d\n", count)
	fmt.Printf("Errors: %d\n", errors)
	fmt.Printf("Rate: %.1f req/sec\n", float64(count)/duration)
}
