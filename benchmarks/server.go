// server.go - Go RouteGuide server for benchmarking

package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"math"
	"net"
	"os"
	"sync"

	"google.golang.org/grpc"
	pb "clgrpc-benchmark/routeguide"
)

type routeGuideServer struct {
	pb.UnimplementedRouteGuideServer
	savedFeatures []*pb.Feature
	routeNotes    map[string][]*pb.RouteNote
	mu            sync.Mutex
}

func (s *routeGuideServer) GetFeature(ctx context.Context, point *pb.Point) (*pb.Feature, error) {
	for _, feature := range s.savedFeatures {
		if feature.Location.Latitude == point.Latitude &&
			feature.Location.Longitude == point.Longitude {
			return feature, nil
		}
	}
	// No feature found, return an unnamed feature
	return &pb.Feature{Location: point}, nil
}

func (s *routeGuideServer) ListFeatures(rect *pb.Rectangle, stream pb.RouteGuide_ListFeaturesServer) error {
	for _, feature := range s.savedFeatures {
		if inRange(feature.Location, rect) {
			if err := stream.Send(feature); err != nil {
				return err
			}
		}
	}
	return nil
}

func (s *routeGuideServer) RecordRoute(stream pb.RouteGuide_RecordRouteServer) error {
	var pointCount, distance int32
	var lastPoint *pb.Point
	startTime := getCurrentTime()

	for {
		point, err := stream.Recv()
		if err == io.EOF {
			endTime := getCurrentTime()
			return stream.SendAndClose(&pb.RouteSummary{
				PointCount:   pointCount,
				Distance:     distance,
				ElapsedTime:  int32(endTime - startTime),
			})
		}
		if err != nil {
			return err
		}
		pointCount++
		if lastPoint != nil {
			distance += calcDistance(lastPoint, point)
		}
		lastPoint = point
	}
}

func (s *routeGuideServer) RouteChat(stream pb.RouteGuide_RouteChatServer) error {
	for {
		note, err := stream.Recv()
		if err == io.EOF {
			return nil
		}
		if err != nil {
			return err
		}

		key := serialize(note.Location)

		s.mu.Lock()
		s.routeNotes[key] = append(s.routeNotes[key], note)
		// Send all previous notes at this location
		notes := make([]*pb.RouteNote, len(s.routeNotes[key]))
		copy(notes, s.routeNotes[key])
		s.mu.Unlock()

		for _, n := range notes {
			if err := stream.Send(n); err != nil {
				return err
			}
		}
	}
}

func inRange(point *pb.Point, rect *pb.Rectangle) bool {
	left := min(rect.Lo.Longitude, rect.Hi.Longitude)
	right := max(rect.Lo.Longitude, rect.Hi.Longitude)
	top := max(rect.Lo.Latitude, rect.Hi.Latitude)
	bottom := min(rect.Lo.Latitude, rect.Hi.Latitude)

	return point.Longitude >= left &&
		point.Longitude <= right &&
		point.Latitude >= bottom &&
		point.Latitude <= top
}

func min(a, b int32) int32 {
	if a < b {
		return a
	}
	return b
}

func max(a, b int32) int32 {
	if a > b {
		return a
	}
	return b
}

func calcDistance(p1, p2 *pb.Point) int32 {
	const coordFactor = 1e7
	const radius = 6371000 // meters

	lat1 := float64(p1.Latitude) / coordFactor
	lat2 := float64(p2.Latitude) / coordFactor
	lng1 := float64(p1.Longitude) / coordFactor
	lng2 := float64(p2.Longitude) / coordFactor

	lat1Rad := toRadians(lat1)
	lat2Rad := toRadians(lat2)
	dLat := toRadians(lat2 - lat1)
	dLng := toRadians(lng2 - lng1)

	a := math.Sin(dLat/2)*math.Sin(dLat/2) +
		math.Cos(lat1Rad)*math.Cos(lat2Rad)*
			math.Sin(dLng/2)*math.Sin(dLng/2)
	c := 2 * math.Atan2(math.Sqrt(a), math.Sqrt(1-a))

	return int32(radius * c)
}

func toRadians(degrees float64) float64 {
	return degrees * math.Pi / 180
}

func serialize(point *pb.Point) string {
	return fmt.Sprintf("%d,%d", point.Latitude, point.Longitude)
}

func getCurrentTime() int64 {
	return 0 // Simplified for benchmark
}

func loadFeatures(filePath string) []*pb.Feature {
	file, err := os.Open(filePath)
	if err != nil {
		log.Printf("Failed to load features: %v", err)
		return nil
	}
	defer file.Close()

	var data []struct {
		Location struct {
			Latitude  int32 `json:"latitude"`
			Longitude int32 `json:"longitude"`
		} `json:"location"`
		Name string `json:"name"`
	}

	if err := json.NewDecoder(file).Decode(&data); err != nil {
		log.Printf("Failed to decode features: %v", err)
		return nil
	}

	features := make([]*pb.Feature, len(data))
	for i, item := range data {
		features[i] = &pb.Feature{
			Name: item.Name,
			Location: &pb.Point{
				Latitude:  item.Location.Latitude,
				Longitude: item.Location.Longitude,
			},
		}
	}

	return features
}

func main() {
	port := 50055
	if len(os.Args) > 1 {
		fmt.Sscanf(os.Args[1], "%d", &port)
	}

	lis, err := net.Listen("tcp", fmt.Sprintf(":%d", port))
	if err != nil {
		log.Fatalf("Failed to listen: %v", err)
	}

	grpcServer := grpc.NewServer()
	features := loadFeatures("../examples/routeguide/route_guide_db.json")

	pb.RegisterRouteGuideServer(grpcServer, &routeGuideServer{
		savedFeatures: features,
		routeNotes:    make(map[string][]*pb.RouteNote),
	})

	fmt.Printf("Go RouteGuide server listening on port %d\n", port)
	if err := grpcServer.Serve(lis); err != nil {
		log.Fatalf("Failed to serve: %v", err)
	}
}
