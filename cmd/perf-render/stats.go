package main

import (
	"math"
	"sort"
	"time"
)

// Statistics holds statistical analysis of frame timings
type Statistics struct {
	MinMs     float64 `json:"min_ms"`
	MaxMs     float64 `json:"max_ms"`
	MeanMs    float64 `json:"mean_ms"`
	P50Ms     float64 `json:"p50_ms"`
	P95Ms     float64 `json:"p95_ms"`
	P99Ms     float64 `json:"p99_ms"`
	StdDevMs  float64 `json:"stddev_ms"`
	TotalMs   float64 `json:"total_ms"`
	ActualFPS float64 `json:"actual_fps"`
}

// calculateStatistics computes statistics from a slice of durations
func calculateStatistics(durations []time.Duration) Statistics {
	if len(durations) == 0 {
		return Statistics{}
	}

	// Convert durations to milliseconds for easier handling
	ms := make([]float64, len(durations))
	var total float64
	for i, d := range durations {
		ms[i] = float64(d.Nanoseconds()) / 1e6
		total += ms[i]
	}

	// Sort for percentile calculations
	sorted := make([]float64, len(ms))
	copy(sorted, ms)
	sort.Float64s(sorted)

	// Calculate statistics
	min := sorted[0]
	max := sorted[len(sorted)-1]
	mean := total / float64(len(ms))

	// Calculate standard deviation
	var variance float64
	for _, v := range ms {
		diff := v - mean
		variance += diff * diff
	}
	stddev := math.Sqrt(variance / float64(len(ms)))

	// Calculate percentiles
	p50 := percentile(sorted, 50)
	p95 := percentile(sorted, 95)
	p99 := percentile(sorted, 99)

	// Calculate actual FPS (frames per second)
	// Total time divided by number of frames
	actualFPS := 0.0
	if total > 0 {
		actualFPS = float64(len(ms)) / (total / 1000.0)
	}

	return Statistics{
		MinMs:     min,
		MaxMs:     max,
		MeanMs:    mean,
		P50Ms:     p50,
		P95Ms:     p95,
		P99Ms:     p99,
		StdDevMs:  stddev,
		TotalMs:   total,
		ActualFPS: actualFPS,
	}
}

// percentile calculates the nth percentile from a sorted slice
func percentile(sorted []float64, p int) float64 {
	if len(sorted) == 0 {
		return 0
	}
	if p <= 0 {
		return sorted[0]
	}
	if p >= 100 {
		return sorted[len(sorted)-1]
	}

	// Use linear interpolation
	rank := float64(p) / 100.0 * float64(len(sorted)-1)
	lower := int(math.Floor(rank))
	upper := int(math.Ceil(rank))

	if lower == upper {
		return sorted[lower]
	}

	// Interpolate between lower and upper
	weight := rank - float64(lower)
	return sorted[lower]*(1-weight) + sorted[upper]*weight
}
