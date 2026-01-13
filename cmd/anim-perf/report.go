package main

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"
)

// printTableReport outputs a human-readable table format
func printTableReport(report Report) {
	fmt.Println("Animation Performance Report")
	fmt.Printf("Terminal Size: %dx%d | Frames: %d | Warmup: %d\n",
		report.Config.Size.R,
		report.Config.Size.C,
		report.Config.Frames,
		report.Config.Warmup,
	)
	fmt.Println()

	// Print header
	fmt.Printf("%-12s %8s %8s %8s %8s %8s %8s %10s\n",
		"Animation", "Min", "Max", "Mean", "P50", "P95", "P99", "FPS")
	fmt.Println(strings.Repeat("-", 82))

	// Print results
	for _, result := range report.Results {
		fmt.Printf("%-12s %7.2fms %7.2fms %7.2fms %7.2fms %7.2fms %7.2fms %10.1f\n",
			truncate(result.Name, 12),
			result.Stats.MinMs,
			result.Stats.MaxMs,
			result.Stats.MeanMs,
			result.Stats.P50Ms,
			result.Stats.P95Ms,
			result.Stats.P99Ms,
			result.Stats.ActualFPS,
		)
	}
	fmt.Println()

	// Print summary statistics if multiple animations
	if len(report.Results) > 1 {
		var totalFrameTime float64
		var fastestFPS float64
		var slowestFPS float64
		var fastestName, slowestName string

		for i, result := range report.Results {
			totalFrameTime += result.Stats.MeanMs
			fps := result.Stats.ActualFPS

			if i == 0 || fps > fastestFPS {
				fastestFPS = fps
				fastestName = result.Name
			}
			if i == 0 || fps < slowestFPS {
				slowestFPS = fps
				slowestName = result.Name
			}
		}

		avgFrameTime := totalFrameTime / float64(len(report.Results))
		fmt.Printf("Summary:\n")
		fmt.Printf("  Average frame time: %.2fms\n", avgFrameTime)
		fmt.Printf("  Fastest: %s (%.1f FPS)\n", fastestName, fastestFPS)
		fmt.Printf("  Slowest: %s (%.1f FPS)\n", slowestName, slowestFPS)
	}
}

// printJSONReport outputs a machine-readable JSON format
func printJSONReport(report Report) error {
	// Create a JSON-friendly structure
	type JSONConfig struct {
		Rows      int `json:"rows"`
		Cols      int `json:"cols"`
		Frames    int `json:"frames"`
		Warmup    int `json:"warmup"`
		TargetFPS int `json:"target_fps"`
	}

	type JSONResult struct {
		Animation string     `json:"animation"`
		Stats     Statistics `json:"stats"`
	}

	type JSONReport struct {
		Timestamp string       `json:"timestamp"`
		Config    JSONConfig   `json:"config"`
		Results   []JSONResult `json:"results"`
	}

	jsonReport := JSONReport{
		Timestamp: report.Timestamp.Format("2006-01-02T15:04:05Z07:00"),
		Config: JSONConfig{
			Rows:      report.Config.Size.R,
			Cols:      report.Config.Size.C,
			Frames:    report.Config.Frames,
			Warmup:    report.Config.Warmup,
			TargetFPS: report.Config.TargetFPS,
		},
		Results: make([]JSONResult, len(report.Results)),
	}

	for i, result := range report.Results {
		jsonReport.Results[i] = JSONResult{
			Animation: result.Name,
			Stats:     result.Stats,
		}
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	return encoder.Encode(jsonReport)
}

// truncate truncates a string to the specified length
func truncate(s string, length int) string {
	if len(s) <= length {
		return s
	}
	return s[:length-3] + "..."
}
