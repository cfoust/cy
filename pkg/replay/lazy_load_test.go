package replay

import (
	"context"
	"fmt"
	"path/filepath"
	"strings"
	"testing"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/replay/player"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/bubbles/cursor"
	"github.com/stretchr/testify/require"
)

func TestLazyLoadCursorUpAtScrollbackLimit(t *testing.T) {
	termSize := geom.Size{R: 3, C: 50}

	var out strings.Builder
	for i := 0; i < 20; i++ {
		out.WriteString(fmt.Sprintf("line %02d\n", i))
	}

	sim := sessions.NewSimulator().
		Add(
			termSize,
			emu.LineFeedMode,
			out.String(),
		)

	borgPath := filepath.Join(t.TempDir(), "test.borg")
	require.NoError(t, sim.WriteBorg(borgPath))

	p := player.New()
	p.SetHistoryLimit(5)
	p.SetRetainOutputData(false)

	for _, event := range sim.Events() {
		require.NoError(t, p.Process(event))
	}

	r := newReplay(
		context.Background(),
		p,
		bind.NewEngine[bind.Action](),
		bind.NewEngine[bind.Action](),
		WithBorgPath(borgPath),
	)

	r.input.Cursor.SetMode(cursor.CursorHide)

	var m taro.Model = r
	run := taro.Test(m, taro.WithKittyKeys)

	run(geom.Size{R: termSize.R + 1, C: termSize.C})

	require.False(t, r.historyLoaded)

	run(ActionEvent{Type: ActionCursorUp})
	run(ActionEvent{Type: ActionBeginning})

	firstAccessible := r.movement.Cursor().R
	require.Greater(t, firstAccessible, 0)
	require.False(t, r.historyLoaded)

	run(ActionEvent{Type: ActionCursorUp})

	require.True(t, r.historyLoaded)
	require.Less(t, r.movement.Cursor().R, firstAccessible)
}

func TestLazyLoadMissingBorgDoesNotHang(t *testing.T) {
	termSize := geom.Size{R: 3, C: 50}

	sim := sessions.NewSimulator().
		Add(
			termSize,
			emu.LineFeedMode,
			"hello\nworld\n",
		)

	borgPath := filepath.Join(t.TempDir(), "missing.borg")

	p := player.New()
	p.SetHistoryLimit(5)
	p.SetRetainOutputData(false)

	for _, event := range sim.Events() {
		require.NoError(t, p.Process(event))
	}

	r := newReplay(
		context.Background(),
		p,
		bind.NewEngine[bind.Action](),
		bind.NewEngine[bind.Action](),
		WithBorgPath(borgPath),
	)

	r.input.Cursor.SetMode(cursor.CursorHide)

	var m taro.Model = r
	run := taro.Test(m, taro.WithKittyKeys)

	run(geom.Size{R: termSize.R + 1, C: termSize.C})

	require.False(t, r.historyLoaded)

	run(ActionEvent{Type: ActionTimeStepBack})

	require.False(t, r.isSeeking)
	require.False(t, r.historyLoaded)
	require.Error(t, r.loadErr)
}

func TestLazyLoadFlushIsCancelable(t *testing.T) {
	termSize := geom.Size{R: 3, C: 50}

	sim := sessions.NewSimulator().
		Add(
			termSize,
			emu.LineFeedMode,
			"hello\nworld\n",
		)

	borgPath := filepath.Join(t.TempDir(), "test.borg")
	require.NoError(t, sim.WriteBorg(borgPath))

	p := player.New()
	p.SetHistoryLimit(5)
	p.SetRetainOutputData(false)

	for _, event := range sim.Events() {
		require.NoError(t, p.Process(event))
	}

	flushed := make(chan struct{}, 1)
	flush := func(ctx context.Context) error {
		flushed <- struct{}{}
		<-ctx.Done()
		return ctx.Err()
	}

	r := newReplay(
		context.Background(),
		p,
		bind.NewEngine[bind.Action](),
		bind.NewEngine[bind.Action](),
		WithBorgPath(borgPath),
		WithBorgFlush(flush),
	)

	r.input.Cursor.SetMode(cursor.CursorHide)

	var m taro.Model = r
	run := taro.Test(m, taro.WithKittyKeys)

	run(geom.Size{R: termSize.R + 1, C: termSize.C})

	run(
		ActionEvent{Type: ActionTimeStepBack},
		ActionEvent{Type: ActionQuit},
	)

	select {
	case <-flushed:
	default:
		require.Fail(t, "expected flush to run")
	}

	require.False(t, r.isSeeking)
	require.False(t, r.historyLoaded)
	require.Nil(t, r.loadErr)
}

func TestLazyStatusBarTimeModeShowsTimeBar(t *testing.T) {
	termSize := geom.Size{R: 3, C: 50}

	sim := sessions.NewSimulator().
		Add(
			termSize,
			emu.LineFeedMode,
			"hello\nworld\n",
		)

	borgPath := filepath.Join(t.TempDir(), "test.borg")
	require.NoError(t, sim.WriteBorg(borgPath))

	p := player.New()
	p.SetHistoryLimit(5)
	p.SetRetainOutputData(false)

	for _, event := range sim.Events() {
		require.NoError(t, p.Process(event))
	}

	r := newReplay(
		context.Background(),
		p,
		bind.NewEngine[bind.Action](),
		bind.NewEngine[bind.Action](),
		WithBorgPath(borgPath),
	)

	r.input.Cursor.SetMode(cursor.CursorHide)

	var m taro.Model = r
	run := taro.Test(m, taro.WithKittyKeys)

	run(geom.Size{R: termSize.R + 1, C: termSize.C})

	require.False(t, r.historyLoaded)

	state := tty.New(geom.Size{R: termSize.R + 1, C: termSize.C})
	r.View(state)

	row := state.Image[state.Image.Size().R-1].String()
	require.NotContains(t, row, "scroll or use time controls to load history")
	require.Contains(t, row, r.params.ReplayTextTimeMode())
	require.Contains(t, row, "[")
}

func TestLazyStatusBarCopyModeKeepsRightSideBlank(t *testing.T) {
	termSize := geom.Size{R: 3, C: 50}

	sim := sessions.NewSimulator().
		Add(
			termSize,
			emu.LineFeedMode,
			"hello\nworld\n",
		)

	borgPath := filepath.Join(t.TempDir(), "test.borg")
	require.NoError(t, sim.WriteBorg(borgPath))

	p := player.New()
	p.SetHistoryLimit(5)
	p.SetRetainOutputData(false)

	for _, event := range sim.Events() {
		require.NoError(t, p.Process(event))
	}

	r := newReplay(
		context.Background(),
		p,
		bind.NewEngine[bind.Action](),
		bind.NewEngine[bind.Action](),
		WithBorgPath(borgPath),
	)

	r.input.Cursor.SetMode(cursor.CursorHide)

	var m taro.Model = r
	run := taro.Test(m, taro.WithKittyKeys)

	run(geom.Size{R: termSize.R + 1, C: termSize.C})
	run(ActionEvent{Type: ActionCursorDown})

	require.False(t, r.historyLoaded)
	require.True(t, r.isCopyMode())

	state := tty.New(geom.Size{R: termSize.R + 1, C: termSize.C})
	r.View(state)

	row := state.Image[state.Image.Size().R-1].String()
	require.NotContains(t, row, "scroll or use time controls to load history")
	require.Contains(t, row, r.params.ReplayTextCopyMode())
	require.NotContains(t, row, "[")
}
