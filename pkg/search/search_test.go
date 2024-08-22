package search

import (
	"context"
	"fmt"
	"path/filepath"
	"testing"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/stretchr/testify/require"
)

var sim = sessions.NewSimulator

func createBorg(path string) error {
	s := sim().Add(geom.DEFAULT_SIZE)

	for i := 0; i < 1000; i++ {
		str := "foo"
		if i%20 == 0 {
			str = "bar"
		}

		s = s.Add(str)
	}

	w, err := sessions.Create(path)
	if err != nil {
		return err
	}

	for _, event := range s.Events() {
		if err := w.Write(event); err != nil {
			return err
		}
	}

	return w.Close()
}

func TestSearch(t *testing.T) {
	paths := []string{}
	for i := 0; i < 5; i++ {
		path := filepath.Join(
			t.TempDir(),
			fmt.Sprintf("foo%d.borg", i),
		)
		require.NoError(
			t,
			createBorg(path),
			"error creating %s",
			path,
		)
		paths = append(paths, path)
	}

	s := newSearch(context.Background())
	test := taro.Test(s)

	test(Request{
		Query:   "bar",
		Files:   paths,
		Workers: 3,
	})
}
