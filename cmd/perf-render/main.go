package main

import (
	"fmt"
	"os"
	"runtime/pprof"
	"time"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/alecthomas/kong"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

var CLI struct {
	Rows   int    `help:"Terminal height in rows" default:"24"`
	Cols   int    `help:"Terminal width in columns" default:"80"`
	Frames int    `help:"Number of frames to measure" default:"300"`
	Warmup int    `help:"Number of warmup frames to skip" default:"30"`
	Format string `help:"Output format: table, json, or both" enum:"table,json,both" default:"table"`
	CPU    string `help:"Path to save CPU profile (pprof format)" type:"path"`
	Memory string `help:"Path to save memory profile (pprof format)" type:"path"`
}

func main() {
	kong.Parse(
		&CLI,
		kong.Name("perf-render"),
		kong.Description(
			"Measure performance of cy's render compositing and diff pipeline.",
		),
		kong.UsageOnError(),
		kong.ConfigureHelp(kong.HelpOptions{
			Compact: true,
			Summary: true,
		}),
	)

	// Set up logging
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
		fmt.Fprintf(
			os.Stderr,
			"Error: rows and cols must be positive\n",
		)
		os.Exit(1)
	}
	if CLI.Frames <= 0 {
		fmt.Fprintf(
			os.Stderr,
			"Error: frames must be positive\n",
		)
		os.Exit(1)
	}
	if CLI.Warmup < 0 {
		fmt.Fprintf(
			os.Stderr,
			"Error: warmup cannot be negative\n",
		)
		os.Exit(1)
	}

	// Start CPU profiling if requested
	if CLI.CPU != "" {
		f, err := os.Create(CLI.CPU)
		if err != nil {
			fmt.Fprintf(
				os.Stderr,
				"Error creating CPU profile: %v\n",
				err,
			)
			os.Exit(1)
		}
		defer func() { _ = f.Close() }()

		if err := pprof.StartCPUProfile(f); err != nil {
			fmt.Fprintf(
				os.Stderr,
				"Error starting CPU profile: %v\n",
				err,
			)
			os.Exit(1)
		}
		defer pprof.StopCPUProfile()
		log.Info().Msgf("CPU profiling enabled: %s", CLI.CPU)
	}

	config := Config{
		Size: geom.Size{
			R: CLI.Rows,
			C: CLI.Cols,
		},
		Frames: CLI.Frames,
		Warmup: CLI.Warmup,
	}

	log.Info().Msgf(
		"Benchmarking render pipeline at %dx%d for %d frames (warmup: %d)",
		CLI.Rows,
		CLI.Cols,
		CLI.Frames,
		CLI.Warmup,
	)

	result, err := runBenchmark(config)
	if err != nil {
		fmt.Fprintf(
			os.Stderr,
			"Error running benchmark: %v\n",
			err,
		)
		os.Exit(1)
	}

	// Write memory profile if requested
	if CLI.Memory != "" {
		f, err := os.Create(CLI.Memory)
		if err != nil {
			fmt.Fprintf(
				os.Stderr,
				"Error creating memory profile: %v\n",
				err,
			)
			os.Exit(1)
		}
		defer func() { _ = f.Close() }()

		_ = pprof.Lookup("heap").WriteTo(f, 0)
		log.Info().Msgf(
			"Memory profile saved: %s",
			CLI.Memory,
		)
	}

	report := Report{
		Timestamp: time.Now(),
		Config:    config,
		Result:    result,
	}

	switch CLI.Format {
	case "table":
		printTableReport(report)
	case "json":
		if err := printJSONReport(report); err != nil {
			fmt.Fprintf(
				os.Stderr,
				"Error generating JSON report: %v\n",
				err,
			)
			os.Exit(1)
		}
	case "both":
		printTableReport(report)
		fmt.Println()
		if err := printJSONReport(report); err != nil {
			fmt.Fprintf(
				os.Stderr,
				"Error generating JSON report: %v\n",
				err,
			)
			os.Exit(1)
		}
	}
}
