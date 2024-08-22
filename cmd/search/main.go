package main

import (
	"compress/gzip"
	"io"
	"os"
	"regexp"
	"time"

	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/sessions/search"

	"github.com/alecthomas/kong"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

var CLI struct {
	Query string   `help:"" short:"q"`
	Files []string `arg:"" type:"existingfile"`
}

func readEvents(filename string) ([]sessions.Event, error) {
	reader, err := sessions.Open(filename)
	if err != nil {
		return nil, err
	}

	events := []sessions.Event{}
	for {
		event, err := reader.Read()
		if err == io.EOF || err == io.ErrUnexpectedEOF {
			break
		}
		if err != nil {
			return nil, err
		}
		events = append(events, event)
	}

	return events, nil
}

func searchFile(query string, filename string) ([]search.SearchResult, error) {
	pattern, err := regexp.Compile(query)
	if err != nil {
		panic(err)
	}

	f, err := os.Open(filename)
	if err != nil {
		return nil, err
	}

	gz, err := gzip.NewReader(f)
	if err != nil {
		return nil, err
	}

	bytes, err := io.ReadAll(gz)
	matches := pattern.FindAllIndex(bytes, -1)
	if len(matches) == 0 {
		return nil, nil
	}

	events, err := readEvents(filename)
	if err != nil {
		return nil, err
	}

	return search.Search(events, query, nil)
}

func main() {
	consoleWriter := zerolog.ConsoleWriter{Out: os.Stdout, TimeFormat: time.RFC3339}
	log.Logger = log.Output(consoleWriter)

	kong.Parse(&CLI,
		kong.Name("search"),
		kong.Description("A small utility to investigate search performance."),
		kong.UsageOnError(),
		kong.ConfigureHelp(kong.HelpOptions{
			Compact: true,
			Summary: true,
		}))

	log.Info().Msgf("searching for %s in %d files", CLI.Query, len(CLI.Files))

	numWorkers := 5
	jobs := make(chan string, numWorkers)
	for i := 0; i < numWorkers; i++ {
		go func() {
			for file := range jobs {
				_, err := searchFile(
					CLI.Query,
					file,
				)
				if err != nil {
					log.Error().Msgf("failed to search in %s", file)
					continue
				}
			}
		}()
	}

	for i, file := range CLI.Files {
		jobs <- file
		log.Info().Msgf("%d/%d", i, len(CLI.Files))
	}
}
