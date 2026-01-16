package main

import (
	"fmt"
	"os"
	"runtime/pprof"
	"sort"
	"time"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"

	"github.com/alecthomas/kong"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

var CLI struct {
	AnimationID string `arg:"" optional:"" help:"Animation ID to test (omit to test all animations)"`
	Rows        int    `help:"Terminal height in rows" default:"24"`
	Cols        int    `help:"Terminal width in columns" default:"80"`
	Frames      int    `help:"Number of frames to measure" default:"300"`
	FPS         int    `help:"Target FPS for context (does not affect actual timing)" default:"30"`
	Warmup      int    `help:"Number of warmup frames to skip" default:"30"`
	Format      string `help:"Output format: table, json, or both" enum:"table,json,both" default:"table"`
	CPU         string `help:"Path to save CPU profile (pprof format)" type:"path"`
	Memory      string `help:"Path to save memory profile (pprof format)" type:"path"`
}

func main() {
	kong.Parse(
		&CLI,
		kong.Name("anim-perf"),
		kong.Description(
			"Measure performance characteristics of cy's animations.",
		),
		kong.UsageOnError(),
		kong.ConfigureHelp(kong.HelpOptions{
			Compact: true,
			Summary: true,
		}),
	)

	// Set up logging - disable for JSON-only output to keep stdout clean
	if CLI.Format == "json" {
		zerolog.SetGlobalLevel(zerolog.Disabled)
	} else {
		consoleWriter := zerolog.ConsoleWriter{
			Out:        os.Stderr,
			TimeFormat: time.RFC3339,
		}
		log.Logger = log.Output(consoleWriter)
	}

	// Validate configuration
	if CLI.Rows <= 0 || CLI.Cols <= 0 {
		fmt.Fprintf(os.Stderr, "Error: rows and cols must be positive\n")
		os.Exit(1)
	}
	if CLI.Frames <= 0 {
		fmt.Fprintf(os.Stderr, "Error: frames must be positive\n")
		os.Exit(1)
	}
	if CLI.Warmup < 0 {
		fmt.Fprintf(os.Stderr, "Error: warmup cannot be negative\n")
		os.Exit(1)
	}

	// Start CPU profiling if requested
	if CLI.CPU != "" {
		f, err := os.Create(CLI.CPU)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error creating CPU profile: %v\n", err)
			os.Exit(1)
		}
		defer func() { _ = f.Close() }()

		if err := pprof.StartCPUProfile(f); err != nil {
			fmt.Fprintf(os.Stderr, "Error starting CPU profile: %v\n", err)
			os.Exit(1)
		}
		defer pprof.StopCPUProfile()
		log.Info().Msgf("CPU profiling enabled: %s", CLI.CPU)
	}

	// Determine which animations to test
	var animationsToTest []string
	if CLI.AnimationID != "" {
		// Validate the animation ID
		if _, ok := anim.Animations[CLI.AnimationID]; !ok {
			fmt.Fprintf(
				os.Stderr,
				"Error: unknown animation '%s'\n\n",
				CLI.AnimationID,
			)
			fmt.Fprintf(os.Stderr, "Available animations:\n")
			availableNames := make([]string, 0, len(anim.Animations))
			for name := range anim.Animations {
				availableNames = append(availableNames, name)
			}
			sort.Strings(availableNames)
			for _, name := range availableNames {
				fmt.Fprintf(os.Stderr, "  - %s\n", name)
			}
			os.Exit(1)
		}
		animationsToTest = []string{CLI.AnimationID}
	} else {
		// Test all animations, sorted alphabetically
		for name := range anim.Animations {
			animationsToTest = append(animationsToTest, name)
		}
		sort.Strings(animationsToTest)
	}

	// Create configuration
	config := Config{
		Size: geom.Size{
			R: CLI.Rows,
			C: CLI.Cols,
		},
		Frames:    CLI.Frames,
		Warmup:    CLI.Warmup,
		TargetFPS: CLI.FPS,
	}

	// Run benchmarks
	log.Info().Msgf(
		"Testing %d animation(s) at %dx%d for %d frames (warmup: %d)",
		len(animationsToTest),
		CLI.Rows,
		CLI.Cols,
		CLI.Frames,
		CLI.Warmup,
	)

	results := make([]Result, 0, len(animationsToTest))
	for _, name := range animationsToTest {
		log.Info().Msgf("Running: %s", name)

		creator := anim.Animations[name]
		animation := creator()

		result, err := runBenchmark(animation, name, config)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error benchmarking %s: %v\n", name, err)
			os.Exit(1)
		}

		results = append(results, result)
	}

	// Write memory profile if requested
	if CLI.Memory != "" {
		f, err := os.Create(CLI.Memory)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error creating memory profile: %v\n", err)
			os.Exit(1)
		}
		defer func() { _ = f.Close() }()

		// Run GC before taking heap snapshot
		_ = pprof.Lookup("heap").WriteTo(f, 0)
		log.Info().Msgf("Memory profile saved: %s", CLI.Memory)
	}

	// Generate report
	report := Report{
		Timestamp: time.Now(),
		Config:    config,
		Results:   results,
	}

	switch CLI.Format {
	case "table":
		printTableReport(report)
	case "json":
		if err := printJSONReport(report); err != nil {
			fmt.Fprintf(os.Stderr, "Error generating JSON report: %v\n", err)
			os.Exit(1)
		}
	case "both":
		printTableReport(report)
		fmt.Println()
		if err := printJSONReport(report); err != nil {
			fmt.Fprintf(os.Stderr, "Error generating JSON report: %v\n", err)
			os.Exit(1)
		}
	}
}

// Config holds the benchmark configuration
type Config struct {
	Size      geom.Size
	Frames    int
	Warmup    int
	TargetFPS int
}

// Result holds the benchmark results for a single animation
type Result struct {
	Name  string
	Stats Statistics
}

// Report holds the complete benchmark report
type Report struct {
	Timestamp time.Time
	Config    Config
	Results   []Result
}

// runBenchmark executes a benchmark for a single animation
func runBenchmark(
	animation anim.Animation,
	name string,
	config Config,
) (Result, error) {
	// Initialize the animation with the specified size
	initialImage := image.New(geom.Vec2{
		R: config.Size.R,
		C: config.Size.C,
	})
	animation.Init(initialImage)

	// Run warmup frames
	var elapsed time.Duration
	for i := 0; i < config.Warmup; i++ {
		elapsed += time.Millisecond * time.Duration(1000/config.TargetFPS)
		animation.Update(elapsed)
	}

	// Run measured frames
	durations := make([]time.Duration, config.Frames)
	for i := 0; i < config.Frames; i++ {
		elapsed += time.Millisecond * time.Duration(1000/config.TargetFPS)

		start := time.Now()
		animation.Update(elapsed)
		durations[i] = time.Since(start)
	}

	// Calculate statistics
	stats := calculateStatistics(durations)

	return Result{
		Name:  name,
		Stats: stats,
	}, nil
}
