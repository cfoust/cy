package main

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"
)

// printTableReport outputs a human-readable table format.
func printTableReport(report Report) {
	fmt.Println("Render Pipeline Benchmark")
	fmt.Printf(
		"Terminal Size: %dx%d | Frames: %d | Warmup: %d\n",
		report.Config.Size.R,
		report.Config.Size.C,
		report.Config.Frames,
		report.Config.Warmup,
	)
	fmt.Println()

	// Print header
	fmt.Printf(
		"%-12s %8s %8s %8s %8s %8s %8s %10s\n",
		"Phase", "Min", "Max", "Mean",
		"P50", "P95", "P99", "FPS",
	)
	fmt.Println(strings.Repeat("-", 82))

	// Print composite stats
	printStatsRow("Composite", report.Result.CompositeStats)
	printStatsRow("Swap", report.Result.SwapStats)

	fmt.Println()

	// Print bandwidth
	bw := report.Result.Bandwidth
	totalMB := float64(bw.TotalBytes) / (1024 * 1024)
	perFrameKB := bw.BytesPerFrame / 1024

	fmt.Println("Bandwidth:")
	fmt.Printf("  Total: %.2f MB over %d frames\n",
		totalMB, bw.FrameCount)
	fmt.Printf("  Per frame: %.2f KB (avg)\n", perFrameKB)
	fmt.Printf("  Throughput: %.2f MB/s\n", bw.MBPerSecond)
}

func printStatsRow(name string, stats Statistics) {
	fmt.Printf(
		"%-12s %7.2fms %7.2fms %7.2fms %7.2fms %7.2fms %7.2fms %10.1f\n",
		name,
		stats.MinMs,
		stats.MaxMs,
		stats.MeanMs,
		stats.P50Ms,
		stats.P95Ms,
		stats.P99Ms,
		stats.ActualFPS,
	)
}

// printJSONReport outputs a machine-readable JSON format.
func printJSONReport(report Report) error {
	type JSONConfig struct {
		Rows   int `json:"rows"`
		Cols   int `json:"cols"`
		Frames int `json:"frames"`
		Warmup int `json:"warmup"`
	}

	type JSONReport struct {
		Timestamp string          `json:"timestamp"`
		Config    JSONConfig      `json:"config"`
		Composite Statistics      `json:"composite"`
		Swap      Statistics      `json:"swap"`
		Bandwidth BandwidthResult `json:"bandwidth"`
	}

	jsonReport := JSONReport{
		Timestamp: report.Timestamp.Format(
			"2006-01-02T15:04:05Z07:00",
		),
		Config: JSONConfig{
			Rows:   report.Config.Size.R,
			Cols:   report.Config.Size.C,
			Frames: report.Config.Frames,
			Warmup: report.Config.Warmup,
		},
		Composite: report.Result.CompositeStats,
		Swap:      report.Result.SwapStats,
		Bandwidth: report.Result.Bandwidth,
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	return encoder.Encode(jsonReport)
}
