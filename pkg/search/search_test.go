package search

import (
	"context"
	"fmt"
	"path/filepath"
	"testing"
	"time"

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

func createTestFiles(t *testing.T) []string {
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
	return paths
}

func TestSearch(t *testing.T) {
	paths := createTestFiles(t)

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

func TestPassthrough(t *testing.T) {
	ctx := context.Background()
	engine := bind.Run(ctx, bind.NewBindScope(nil))
	timeBinds := bind.NewBindScope(nil)
	timeBinds.Set([]interface{}{
		"a",
	}, bind.Action{
		Tag: "foo",
	})
	s := newSearch(
		ctx,
		engine,
		timeBinds,
		bind.NewBindScope(nil),
	)

	// We need to ensure that the request is complete before testing
	// other things, so we use taro.Test
	test := taro.Test(s)
	paths := createTestFiles(t)
	request := Request{
		Query:   "bar",
		Files:   paths,
		Workers: 3,
	}
	test(request)

	program := taro.New(ctx, s)

	// Reinitialize the replay watcher manually
	program.Send(taro.NewWatcher(ctx, s.replay).Wait()())

	actions := make(chan bind.BindEvent)
	go func() {
		events := program.Subscribe(ctx)
		for {
			select {
			case <-ctx.Done():
				return
			case event := <-events.Recv():
				if action, ok := event.(bind.BindEvent); ok {
					actions <- action
				}
			}
		}
	}()

	// Wait for Replay to start up
	time.Sleep(500 * time.Millisecond)

	for _, key := range taro.KeysToMsg("a") {
		program.Send(key)
	}
	event := <-actions
	require.Equal(t, "foo", event.Action.Tag)
}
