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
	conn, err := grpc.Dial("localhost:50055", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("Failed to connect: %v", err)
	}
	defer conn.Close()

	client := pb.NewRouteGuideClient(conn)
	ctx := context.Background()
	point := &pb.Point{Latitude: 409146138, Longitude: -746188906}

	fmt.Println("Sending 100 requests in a loop...")
	start := time.Now()
	
	for i := 0; i < 100; i++ {
		reqStart := time.Now()
		_, err := client.GetFeature(ctx, point)
		reqDur := time.Since(reqStart)
		
		if err != nil {
			fmt.Printf("Request %d FAILED: %v (took %v)\n", i+1, err, reqDur)
		}
		
		if i < 10 || i >= 90 {
			fmt.Printf("Request %3d: %v\n", i+1, reqDur)
		} else if i == 10 {
			fmt.Println("  ... (requests 11-90)")
		}
	}
	
	total := time.Since(start)
	fmt.Printf("\nTotal: %v\n", total)
	fmt.Printf("Average: %v per request\n", total/100)
	fmt.Printf("Rate: %.1f req/sec\n", 100.0/total.Seconds())
}
