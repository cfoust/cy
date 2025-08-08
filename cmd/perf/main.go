package main

import (
	"fmt"
	"io"
	"os"
	"runtime/pprof"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/sessions/search"

	"github.com/alecthomas/kong"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

const (
	B  uint64 = 1
	KB        = B << 10
	MB        = KB << 10
	GB        = MB << 10
)

var CLI struct {
	Query string `help:"" optional:""`
	Limit int    `help:"Limit the number of events."`

	Borg struct {
		Path string `arg:"" name:"path" help:".borg file to search inside." type:"path"`
	} `cmd:"" help:"Search inside a borg file."`

	Fake struct{} `cmd:"" help:"Search inside generated data."`
}

func main() {
	consoleWriter := zerolog.ConsoleWriter{
		Out:        os.Stdout,
		TimeFormat: time.RFC3339,
	}
	log.Logger = log.Output(consoleWriter)

	ctx := kong.Parse(&CLI,
		kong.Name("perf"),
		kong.Description("A small utility to benchmark search performance."),
		kong.UsageOnError(),
		kong.ConfigureHelp(kong.HelpOptions{
			Compact: true,
			Summary: true,
		}))

	var events []sessions.Event

	switch ctx.Command() {
	case "borg <path>":
		reader, err := sessions.Open(CLI.Borg.Path)
		if err != nil {
			panic(err)
		}

		for {
			event, err := reader.Read()
			if err == io.EOF || err == io.ErrUnexpectedEOF {
				break
			}
			if err != nil {
				panic(err)
			}
			events = append(events, event)
		}

	case "fake":
		sim := sessions.NewSimulator()
		sim.Add(
			emu.LineFeedMode,
			geom.DEFAULT_SIZE,
		)

		var numBytes, desiredBytes uint64
		desiredBytes = 2 * MB
		i := 0
		numMatches := 0
		for numBytes < desiredBytes {
			s := "foo"
			if (i % 100) == 0 {
				numMatches++
				s = "bar"
			}
			sim.Add(s)
			numBytes += uint64(len([]byte(s)))
			i++
		}

		fmt.Printf("%d matches in %d bytes", numMatches, desiredBytes)
		events = sim.Events()
	default:
		panic(ctx.Command())
	}

	if CLI.Limit != 0 {
		events = events[:CLI.Limit]
	}

	log.Info().Msgf("searching for %s in %d events", CLI.Query, len(events))

	f, err := os.Create("search.prof")
	if err != nil {
		panic(err)
	}
	defer func() { _ = f.Close() }()
	if err := pprof.StartCPUProfile(f); err != nil {
		panic(err)
	}
	defer pprof.StopCPUProfile()

	results, err := search.Search(events, CLI.Query, nil)
	if err != nil {
		panic(err)
	}

	log.Info().Msgf("found %d results", len(results))
}
