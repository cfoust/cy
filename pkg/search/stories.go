package search

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/stories"
)

const (
	STORY_BORG_FILE = "/tmp/search-story.borg"
)

func createStoryBorg(path string) error {
	if _, err := os.Stat(path); !os.IsNotExist(err) {
		return nil
	}

	s := sessions.NewSimulator().
		Defaults()

	for i := 0; i < 100_000; i++ {
		str := "foo\n"
		if i%5000 == 0 {
			str = "bar"
		}

		s = s.Add(str)
	}

	return s.WriteBorg(path)
}

func createBorgFiles(count int) ([]string, error) {
	var paths []string
	for i := 0; i < count; i++ {
		path := filepath.Join(
			"/tmp",
			fmt.Sprintf("search-story%d.borg", i),
		)

		err := createStoryBorg(path)
		if err != nil {
			return nil, err
		}

		paths = append(paths, path)
	}

	return paths, nil
}

var Load stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	paths, err := createBorgFiles(5)
	if err != nil {
		return nil, err
	}

	return New(
		ctx,
		bind.NewBindScope(nil),
		bind.NewBindScope(nil),
		bind.NewBindScope(nil),
		WithRequest(Request{
			Query:   "bar",
			Files:   paths,
			Workers: 3,
		}),
	), nil
}

func init() {
	stories.Register(
		"search/query",
		Load,
		stories.Config{},
	)
}
