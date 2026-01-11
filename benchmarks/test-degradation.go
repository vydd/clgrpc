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

	fmt.Println("Testing degradation pattern...")
	
	for batch := 1; batch <= 5; batch++ {
		fmt.Printf("\n=== Batch %d (requests %d-%d) ===\n", batch, (batch-1)*20+1, batch*20)
		
		var sum time.Duration
		var max time.Duration
		var min time.Duration = time.Hour
		
		for i := 0; i < 20; i++ {
			reqStart := time.Now()
			_, err := client.GetFeature(ctx, point)
			reqDur := time.Since(reqStart)
			
			if err != nil {
				fmt.Printf("  Request %d FAILED: %v\n", (batch-1)*20+i+1, err)
			}
			
			sum += reqDur
			if reqDur > max {
				max = reqDur
			}
			if reqDur < min {
				min = reqDur
			}
		}
		
		avg := sum / 20
		fmt.Printf("  Min: %v, Avg: %v, Max: %v\n", min, avg, max)
	}
}
