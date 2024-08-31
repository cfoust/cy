package search

import (
	"context"
	"fmt"
	"path/filepath"
	"testing"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/stretchr/testify/require"
)

var sim = sessions.NewSimulator

func createBorg(path string) error {
	s := sim().Add(geom.DEFAULT_SIZE)

	for i := 0; i < 100; i++ {
		str := "foo"
		if i%20 == 0 {
			str = "bar"
		}

		s = s.Add(str)
	}

	return s.WriteBorg(path)
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

	ctx := context.Background()
	engine := bind.Run(ctx, bind.NewBindScope(nil))
	s := newSearch(
		ctx,
		engine,
		bind.NewBindScope(nil),
		bind.NewBindScope(nil),
	)

	test := taro.Test(s)
	request := Request{
		Query:   "bar",
		Files:   paths,
		Workers: 3,
	}

	test(request)

	require.Equal(t, len(paths), len(s.complete))
	for _, result := range s.complete {
		require.True(t, result.Done)
	}
	require.False(t, s.searching)

	test(request, ActionEvent{Type: ActionCancel})
	require.False(t, s.searching)

	// Should leave complete untouched
	require.Equal(t, len(paths), len(s.complete))
	for _, result := range s.complete {
		require.True(t, result.Done)
	}
}
