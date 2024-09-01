package search

import (
	"compress/gzip"
	"context"
	"io"
	"os"
	"regexp"
	"sync"

	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/sessions/search"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
)

type Request struct {
	Query string
	Files []string
	// The number of goroutines to use
	Workers int
}

type fileResult struct {
	ID      int
	File    string
	Done    bool
	Error   error
	Results []search.SearchResult
}

type resultEvent struct {
	fileResult
}

type job struct {
	ID   int
	File string
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

func searchFile(
	query string,
	filename string,
) ([]search.SearchResult, error) {
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

func work(
	ctx context.Context,
	query string,
	jobs <-chan job,
	out chan<- fileResult,
) {
	for {
		select {
		case <-ctx.Done():
			return
		case job, more := <-jobs:
			if !more {
				return
			}

			results, err := searchFile(
				query,
				job.File,
			)

			select {
			case <-ctx.Done():
				return
			case out <- fileResult{
				ID:      job.ID,
				File:    job.File,
				Done:    true,
				Error:   err,
				Results: results,
			}:
			}
		}
	}
}

func execute(
	ctx context.Context,
	query string,
	numWorkers int,
	jobs []job,
	out chan<- fileResult,
) tea.Cmd {
	var wg sync.WaitGroup
	jobc := make(chan job, numWorkers)

	return func() tea.Msg {
		for i := 0; i < numWorkers; i++ {
			wg.Add(1)
			go func() {
				defer wg.Done()
				work(ctx, query, jobc, out)
			}()
		}

		for _, job := range jobs {
			select {
			case <-ctx.Done():
				return nil
			case jobc <- job:
			}
		}

		close(jobc)
		wg.Wait()
		return nil
	}
}

func (s *Search) waitResult() tea.Cmd {
	lifetime := s.searchLifetime
	if s.resultc == nil || lifetime == nil {
		return nil
	}

	return func() tea.Msg {
		select {
		case <-lifetime.Ctx().Done():
			return nil
		case result, more := <-s.resultc:
			if !more {
				return nil
			}
			return resultEvent{
				fileResult: result,
			}
		}

	}
}

func (s *Search) handleResult(event resultEvent) (taro.Model, tea.Cmd) {
	s.pending[event.fileResult.ID] = event.fileResult

	allDone := true
	for _, result := range s.pending {
		if result.Done {
			continue
		}
		allDone = false
	}

	if !allDone {
		return s, s.waitResult()
	}

	var complete []fileResult
	for _, result := range s.pending {
		if len(result.Results) == 0 {
			continue
		}
		complete = append(complete, result)
	}

	s.complete = complete
	s.query = s.pendingQuery
	s.pendingQuery = ""
	s.pending = nil
	s.cancelSearch()
	return s, s.setSelected(0)
}

func (s *Search) cancelSearch() {
	if s.searchLifetime != nil {
		s.searchLifetime.Cancel()
	}
	s.resultc = nil
	s.searching = false
}

func (s *Search) Execute(request Request) (taro.Model, tea.Cmd) {
	s.cancelSearch()

	if request.Workers == 0 || len(request.Files) == 0 {
		return s, nil
	}

	l := util.NewLifetime(s.Lifetime.Ctx())
	s.searchLifetime = &l
	s.searching = true
	s.pendingQuery = request.Query

	resultc := make(chan fileResult, len(request.Files))
	jobs := make([]job, len(request.Files))
	s.resultc = resultc
	s.pending = make([]fileResult, len(request.Files))
	for id, file := range request.Files {
		s.pending[id].ID = id
		s.pending[id].File = file
		jobs[id].ID = id
		jobs[id].File = file
	}

	return s, tea.Batch(
		execute(
			l.Ctx(),
			request.Query,
			request.Workers,
			jobs,
			resultc,
		),
		s.waitResult(),
	)
}
