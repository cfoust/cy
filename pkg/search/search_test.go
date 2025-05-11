package search

import (
	"context"
	"fmt"
	"path/filepath"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay"
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

func createTest() *Search {
	ctx := context.Background()
	engine := bind.Run(ctx, bind.NewBindScope(nil))
	return newSearch(
		ctx,
		engine,
		bind.NewBindScope(nil),
		bind.NewBindScope(nil),
	)
}

func TestSearch(t *testing.T) {
	s := createTest()

	paths := createTestFiles(t)
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
	program.Send(taro.NewWatcher(ctx, s.loader).Wait()())

	actions := make(chan bind.BindEvent)
	copies := make(chan replay.CopyEvent)
	go func() {
		events := program.Subscribe(ctx)

		gotEvent := false
		for {
			select {
			case <-ctx.Done():
				return
			case event := <-events.Recv():
				if action, ok := event.(bind.BindEvent); ok {
					// There was a bug where we would get
					// the same event over and over; this
					// catches that, albeit in a naive
					// way
					if gotEvent {
						t.Fatalf("duplicate event: %+v", action)
					}

					gotEvent = true
					actions <- action
				}

				if _copy, ok := event.(replay.CopyEvent); ok {
					copies <- _copy
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

	program.Send(replay.ActionEvent{Type: replay.ActionCursorLeft})
	program.Send(replay.ActionEvent{Type: replay.ActionSelect})
	program.Send(replay.CopyEvent{})
	ctx, cancel := context.WithTimeout(ctx, time.Second)
	defer cancel()
	select {
	case _copy := <-copies:
		require.Equal(t, "r", _copy.Text)
	case <-ctx.Done():
		t.Fatalf("timeout waiting for copy")
	}
}

func TestSelected(t *testing.T) {
	s := createTest()
	paths := createTestFiles(t)

	test := taro.Test(s)
	request := Request{
		Query:   "bar",
		Files:   paths,
		Workers: 3,
	}
	test(request)

	test(ActionEvent{Type: ActionNext})
	require.Equal(t, 1, s.selected)

	test(ActionEvent{Type: ActionPrev})
	require.Equal(t, 0, s.selected)

	test(ActionEvent{Type: ActionLast})
	require.Equal(t, 4, s.selected)

	test(ActionEvent{Type: ActionFirst})
	require.Equal(t, 0, s.selected)

	test(ActionEvent{Type: ActionPrev})
	require.Equal(t, 4, s.selected)

	test(ActionEvent{Type: ActionNext})
	require.Equal(t, 0, s.selected)
}

func TestInput(t *testing.T) {
	s := createTest()
	paths := createTestFiles(t)

	test := taro.Test(s)
	request := Request{
		Query:   "bar",
		Files:   paths,
		Workers: 3,
	}
	test(request)

	// Full search flow
	test(
		ActionEvent{Type: ActionInput},
		"test",
	)
	require.Equal(t, "test", s.input.Value())
	test("enter")
	require.Equal(t, 0, len(s.complete))

	// Reset, cancel search
	test(request)
	test(
		ActionEvent{Type: ActionInput},
		"asd",
		"ctrl+c",
	)
	require.False(t, s.inputing)
	// should not change existing search
	require.Equal(t, 5, len(s.complete))

	// Non-regex
	test(
		ActionEvent{Type: ActionInput},
		"asdasd[",
		"enter",
	)
}
