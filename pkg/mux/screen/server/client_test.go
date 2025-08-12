package server

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"

	"github.com/stretchr/testify/require"
)

type testScreen struct {
	*mux.UpdatePublisher
	state *tty.State
}

func (m *testScreen) State() *tty.State {
	return m.state
}

func (m *testScreen) Resize(size mux.Size) error {
	return nil
}

func (m *testScreen) Kill() {
}

func (m *testScreen) Send(message mux.Msg) {
}

func newTestScreen(size geom.Vec2) *testScreen {
	return &testScreen{
		UpdatePublisher: mux.NewPublisher(),
		state:           tty.New(size),
	}
}

func TestResize(t *testing.T) {
	var (
		ctx        = t.Context()
		server     = &Server{}
		smallSize  = geom.Vec2{R: 10, C: 20}
		clientSize = geom.Vec2{R: 30, C: 40}
		largeSize  = geom.Vec2{R: 50, C: 60}
		mockScreen = newTestScreen(smallSize)
		client     = &Client{
			size:            clientSize,
			UpdatePublisher: mux.NewPublisher(),
			server:          server,
		}
	)

	client.Attach(ctx, mockScreen)

	// Client larger than screen
	state := client.State()
	require.Equal(t, clientSize, state.Image.Size())
	{
		client.RLock()
		screenPos := client.screenPos
		client.RUnlock()
		require.Equal(
			t,
			clientSize.Center(smallSize),
			screenPos,
		)
	}

	// Screen larger than client
	largeScreen := newTestScreen(largeSize)
	client.Attach(ctx, largeScreen)
	state = client.State()
	require.Equal(t, clientSize, state.Image.Size())
	{
		client.RLock()
		screenPos := client.screenPos
		client.RUnlock()
		require.Equal(
			t,
			clientSize.Center(largeSize),
			screenPos,
		)
	}
}

func TestCursorAtEdges(t *testing.T) {
	var (
		ctx        = t.Context()
		server     = &Server{}
		clientSize = geom.Vec2{R: 10, C: 20}
		screen     = newTestScreen(clientSize)
		client     = &Client{
			size:            clientSize,
			UpdatePublisher: mux.NewPublisher(),
			server:          server,
		}
	)

	screen.state.Cursor = emu.Cursor{Vec2: geom.Vec2{R: 9, C: 19}}
	screen.state.CursorVisible = true

	client.Attach(ctx, screen)
	state := client.State()

	require.True(
		t,
		state.CursorVisible,
		"cursor at bottom-right edge should be visible",
	)
}
