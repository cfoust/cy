package engine

import (
	"context"
	"os"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/keys"
	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/mux"
	S "github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	T "github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/sasha-s/go-deadlock"
	"github.com/stretchr/testify/require"
)

// capScreen is a mock mux.Screen that captures all messages sent to it
// and tracks resize events. It's used for end-to-end mouse event testing.
type capScreen struct {
	deadlock.Mutex
	*mux.UpdatePublisher
	size geom.Size
	msgs []mux.Msg
}

var _ mux.Screen = (*capScreen)(nil)

func newCapScreen() *capScreen {
	return &capScreen{
		UpdatePublisher: mux.NewPublisher(),
		size:            geom.DEFAULT_SIZE,
	}
}

func (c *capScreen) State() *tty.State {
	c.Lock()
	defer c.Unlock()
	return tty.New(c.size)
}

func (c *capScreen) Resize(size geom.Size) error {
	c.Lock()
	c.size = size
	c.Unlock()
	return nil
}

func (c *capScreen) Kill() {}

func (c *capScreen) Send(msg mux.Msg) {
	c.Lock()
	defer c.Unlock()
	c.msgs = append(c.msgs, msg)
}

func (c *capScreen) getMsgs() []mux.Msg {
	c.Lock()
	defer c.Unlock()
	out := make([]mux.Msg, len(c.msgs))
	copy(out, c.msgs)
	return out
}

func (c *capScreen) clearMsgs() {
	c.Lock()
	defer c.Unlock()
	c.msgs = nil
}

func TestSet(t *testing.T) {
	l := New(
		context.Background(),
		T.NewTree(),
		server.New(),
	)

	err := l.Set(L.New(
		&L.MarginsNode{
			Node: &L.SplitNode{
				A: &L.ViewNode{Attached: true},
				B: &L.ViewNode{},
			},
		},
	))
	require.NoError(t, err)

	before := l.existing
	err = l.Set(L.New(
		&L.MarginsNode{
			Node: &L.SplitNode{
				A: &L.ViewNode{Attached: true},
				B: &L.SplitNode{
					A: &L.ViewNode{},
					B: &L.ViewNode{},
				},
			},
		},
	))
	require.NoError(t, err)
	require.NotEqual(t, before.Config, l.existing.Config)
}

func TestClickInactivePane(t *testing.T) {
	size := geom.DEFAULT_SIZE
	l := New(
		context.Background(),
		T.NewTree(),
		server.New(),
	)
	_ = l.Resize(size)

	err := l.Set(L.New(
		&L.SplitNode{
			A: &L.ViewNode{Attached: true},
			B: &L.ViewNode{},
		},
	))
	require.NoError(t, err)

	l.Send(taro.MouseMsg{
		Vec2: geom.Vec2{
			R: 0,
			C: 45,
		},
		Type:   keys.MousePress,
		Button: keys.MouseLeft,
	})
	time.Sleep(500 * time.Millisecond)

	require.Equal(t, &L.SplitNode{
		A: &L.ViewNode{},
		B: &L.ViewNode{Attached: true},
	}, l.Get().Root)
}

func TestViewRemoval(t *testing.T) {
	ctx := context.Background()
	size := geom.DEFAULT_SIZE
	tree := T.NewTree()
	params := params.New()
	l := New(
		ctx,
		tree,
		server.New(),
		WithParams(params),
	)
	_ = l.Resize(size)

	createView := func() (*taro.Program, *T.Pane, *T.NodeID) {
		static := L.NewStatic(
			ctx,
			false,
			"foo",
		)
		pane := tree.Root().NewPane(ctx, static)
		id := pane.Id()
		return static, pane, &id
	}

	// Suffice it to say that I'm not happy about this, but it turns out
	// that there are a lot of sensitive timing issues here and fixing this
	// with a bunch of channels shoved into the LayoutEngine added a lot of
	// complexity. It's not only a matter of waiting for the layout to be
	// updated; you must also ensure that all nodes all the way down the
	// tree have all started the goroutines they need to function. Worth a
	// revisit someday if this is still flaky.
	sleepDuration := 500 * time.Millisecond
	if _, ok := os.LookupEnv("CI"); ok {
		sleepDuration = 2 * time.Second
	}
	sleep := func() {
		time.Sleep(sleepDuration)
	}

	// First we just test killing the tree nodes
	{
		_, pane1, id1 := createView()
		_, pane2, id2 := createView()

		removeOnExit := true
		err := l.Set(L.New(
			&L.SplitNode{
				A: &L.ViewNode{
					Attached: true,
					ID:       id1,
				},
				B: &L.ViewNode{
					ID:           id2,
					RemoveOnExit: &removeOnExit,
				},
			},
		))
		require.NoError(t, err)

		sleep()
		pane1.Cancel()
		sleep()

		// The attached node should stick around
		require.Equal(t, &L.SplitNode{
			A: &L.ViewNode{
				Attached: true,
				ID:       nil,
			},
			B: &L.ViewNode{
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}, l.Get().Root)

		// Switch attachment to B
		require.NoError(t, l.Set(L.New(
			&L.SplitNode{
				A: &L.ViewNode{
					ID: nil,
				},
				B: &L.ViewNode{
					Attached:     true,
					ID:           id2,
					RemoveOnExit: &removeOnExit,
				},
			},
		)))

		// This pane should not stick around
		pane2.Cancel()
		sleep()
		require.Equal(t, &L.ViewNode{
			Attached: true,
			ID:       nil,
		}, l.Get().Root)
	}

	// Next we test exiting the panes themselves
	{
		static1, _, id1 := createView()
		static2, _, id2 := createView()

		removeOnExit := true
		layout := &L.SplitNode{
			A: &L.ViewNode{
				Attached: true,
				ID:       id1,
			},
			B: &L.ViewNode{
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}

		require.NoError(t, l.Set(L.New(layout)))

		// If static1 exits without an error and removeOnExit is not
		// true, nothing should happen.
		static1.Publish(S.ExitEvent{})
		sleep()
		require.Equal(t, &L.SplitNode{
			A: &L.ViewNode{
				Attached: true,
				ID:       id1,
			},
			B: &L.ViewNode{
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}, l.Get().Root)

		// Reset the layout to force new subscriptions
		require.NoError(t, l.Set(L.New(&L.ViewNode{Attached: true})))
		require.NoError(t, l.Set(L.New(layout)))

		// static1 exiting with an error should still cause nothing to happen
		static1.Publish(S.ExitEvent{
			Errored: true,
		})
		sleep()
		require.Equal(t, &L.SplitNode{
			A: &L.ViewNode{
				Attached: true,
				ID:       id1,
			},
			B: &L.ViewNode{
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}, l.Get().Root)

		layout = &L.SplitNode{
			A: &L.ViewNode{
				ID: id1,
			},
			B: &L.ViewNode{
				Attached:     true,
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}
		require.NoError(t, l.Set(L.New(&L.ViewNode{Attached: true})))
		require.NoError(t, l.Set(L.New(layout)))
		sleep()

		// If static2 exits without an error, it should remove the node
		static2.Publish(S.ExitEvent{})
		sleep()
		require.Equal(t, &L.ViewNode{
			Attached: true,
			ID:       id1,
		}, l.Get().Root)

		// Reset
		require.NoError(t, l.Set(L.New(&L.ViewNode{Attached: true})))
		require.NoError(t, l.Set(L.New(layout)))

		// If static2 exits with an error, it should NOT remove the node
		static2.Publish(S.ExitEvent{Errored: true})
		sleep()
		require.Equal(t, layout, l.Get().Root)
	}
}

func TestClickStack(t *testing.T) {
	size := geom.DEFAULT_SIZE
	l := New(
		context.Background(),
		T.NewTree(),
		server.New(),
	)
	_ = l.Resize(size)

	err := l.Set(L.New(
		&L.StackNode{
			Leaves: []L.Leaf{
				{
					Active: true,
					Node:   &L.ViewNode{Attached: true},
				},
				{
					Node: &L.ViewNode{},
				},
			},
		},
	))
	require.NoError(t, err)

	// Force state computation
	l.State()

	click := func(loc geom.Vec2) {
		l.Send(taro.MouseMsg{
			Vec2:   loc,
			Type:   keys.MousePress,
			Button: keys.MouseLeft,
		})
		time.Sleep(500 * time.Millisecond)
	}

	// Click on the collapsed leaf below (last row area)
	// With 2 leaves and active=0, the layout is:
	// row 0: top border of active
	// rows 1..R-3: inner content
	// row R-2: bottom border of active
	// row R-1: collapsed leaf 1 (below)
	click(geom.Vec2{R: size.R - 1, C: 5})
	result := l.Get().Root.(*L.StackNode)
	require.False(t, result.Leaves[0].Active)
	require.True(t, result.Leaves[1].Active)
	require.True(t, result.Leaves[1].Node.IsAttached())
}

func TestClickTabs(t *testing.T) {
	size := geom.DEFAULT_SIZE
	l := New(
		context.Background(),
		T.NewTree(),
		server.New(),
	)
	_ = l.Resize(size)

	name := "tab"
	err := l.Set(L.New(
		&L.TabsNode{
			Tabs: []L.Tab{
				{
					Name:   name,
					Active: true,
					Node:   &L.ViewNode{Attached: true},
				},
				{
					Name:   name,
					Active: false,
					Node:   &L.ViewNode{},
				},
			},
		},
	))
	require.NoError(t, err)

	// Required to force the bar bounds to calculate
	l.State()

	click := func(loc geom.Vec2) {
		l.Send(taro.MouseMsg{
			Vec2:   loc,
			Type:   keys.MousePress,
			Button: keys.MouseLeft,
		})
		time.Sleep(500 * time.Millisecond)
	}

	// Should be on the second tab
	click(geom.Vec2{C: 5})
	require.Equal(t, &L.TabsNode{
		Tabs: []L.Tab{
			{
				Name:   name,
				Active: false,
				Node:   &L.ViewNode{},
			},
			{
				Name:   name,
				Active: true,
				Node:   &L.ViewNode{Attached: true},
			},
		},
	}, l.Get().Root)

	// Should be on the first tab
	click(geom.Vec2{C: 1})
	require.Equal(t, &L.TabsNode{
		Tabs: []L.Tab{
			{
				Name:   name,
				Active: true,
				Node:   &L.ViewNode{Attached: true},
			},
			{
				Name:   name,
				Active: false,
				Node:   &L.ViewNode{},
			},
		},
	}, l.Get().Root)

	// This should not cause any trouble
	click(geom.Vec2{C: 1})
	require.Equal(t, &L.TabsNode{
		Tabs: []L.Tab{
			{
				Name:   name,
				Active: true,
				Node:   &L.ViewNode{Attached: true},
			},
			{
				Name:   name,
				Active: false,
				Node:   &L.ViewNode{},
			},
		},
	}, l.Get().Root)

	// Now set nested tabs
	err = l.Set(L.New(
		&L.TabsNode{
			Tabs: []L.Tab{
				{
					Name:   name,
					Active: true,
					Node:   &L.ViewNode{Attached: true},
				},
				{
					Name:   name,
					Active: false,
					Node: &L.TabsNode{
						Tabs: []L.Tab{
							{
								Name:   name,
								Active: true,
								Node:   &L.ViewNode{},
							},
						},
					},
				},
			},
		},
	))
	require.NoError(t, err)
	click(geom.Vec2{C: 5})

	require.Equal(t, &L.TabsNode{
		Tabs: []L.Tab{
			{
				Name:   name,
				Active: false,
				Node:   &L.ViewNode{},
			},
			{
				Name:   name,
				Active: true,
				Node: &L.TabsNode{
					Tabs: []L.Tab{
						{
							Name:   name,
							Active: true,
							Node:   &L.ViewNode{Attached: true},
						},
					},
				},
			},
		},
	}, l.Get().Root)
}

// TestMouseForwardAllRows verifies that mouse events at every row are
// forwarded through the full layout pipeline to the final screen.
// This is an end-to-end test: LayoutEngine → Margins → layout.Pane →
// server.Client → tree pane's screen (capScreen).
func TestMouseForwardAllRows(t *testing.T) {
	var (
		ctx  = context.Background()
		tree = T.NewTree()
		srv  = server.New()
	)

	testCase := func(
		t *testing.T,
		size geom.Size,
		layout L.Node,
		screen *capScreen,
	) {
		l := New(ctx, tree, srv)
		require.NoError(t, l.Resize(size))
		require.NoError(t, l.Set(L.New(layout)))

		// Force state computation so server.Client computes screenPos
		time.Sleep(100 * time.Millisecond)
		_ = l.State()

		for row := range size.R {
			screen.clearMsgs()

			l.Send(taro.MouseMsg{
				Vec2:   geom.Vec2{R: row, C: size.C / 2},
				Type:   keys.MousePress,
				Button: keys.MouseLeft,
				Down:   true,
			})

			msgs := screen.getMsgs()
			require.Len(t, msgs, 1,
				"row %d: expected one forwarded message", row)

			mouse, ok := msgs[0].(taro.MouseMsg)
			require.True(t, ok,
				"row %d: should be MouseMsg", row)
			require.Equal(t, row, mouse.R,
				"row %d: row should be preserved", row)
		}
		l.Kill()
	}

	t.Run("margins-80cols-exact", func(t *testing.T) {
		screen := newCapScreen()
		pane := tree.Root().NewPane(ctx, screen)
		id := pane.Id()
		testCase(t, geom.Size{R: 26, C: 80}, &L.MarginsNode{
			Cols: 80,
			Node: &L.ViewNode{Attached: true, ID: &id},
		}, screen)
	})

	t.Run("margins-80cols-wide-terminal", func(t *testing.T) {
		screen := newCapScreen()
		pane := tree.Root().NewPane(ctx, screen)
		id := pane.Id()
		testCase(t, geom.Size{R: 40, C: 160}, &L.MarginsNode{
			Cols: 80,
			Node: &L.ViewNode{Attached: true, ID: &id},
		}, screen)
	})

	t.Run("margins-default-layout", func(t *testing.T) {
		screen := newCapScreen()
		pane := tree.Root().NewPane(ctx, screen)
		id := pane.Id()
		def := L.Default().Root.(*L.MarginsNode)
		def.Node = &L.ViewNode{Attached: true, ID: &id}
		testCase(t, geom.Size{R: 26, C: 120}, def, screen)
	})

	// When Rows is set, the pane has a fixed row count and
	// inner.Position.R > 0. Events outside the inner area should be
	// dropped; events inside should arrive with translated coordinates.
	t.Run("margins-fixed-rows", func(t *testing.T) {
		var (
			screen = newCapScreen()
			size   = geom.Size{R: 40, C: 120}
		)
		pane := tree.Root().NewPane(ctx, screen)
		id := pane.Id()

		l := New(ctx, tree, srv)
		require.NoError(t, l.Resize(size))
		require.NoError(t, l.Set(L.New(&L.MarginsNode{
			Cols: 80,
			Rows: 20,
			Node: &L.ViewNode{Attached: true, ID: &id},
		})))

		time.Sleep(100 * time.Millisecond)
		_ = l.State()

		// inner area: 20 rows centered in 40 → rows 10..29
		innerTop := (size.R - 20) / 2
		innerBot := innerTop + 20

		for row := range size.R {
			screen.clearMsgs()

			l.Send(taro.MouseMsg{
				Vec2:   geom.Vec2{R: row, C: size.C / 2},
				Type:   keys.MousePress,
				Button: keys.MouseLeft,
				Down:   true,
			})

			msgs := screen.getMsgs()

			if row < innerTop || row >= innerBot {
				require.Empty(t, msgs,
					"row %d: outside inner area, should be dropped", row)
				continue
			}

			require.Len(t, msgs, 1,
				"row %d: inside inner area, expected one message", row)
			mouse, ok := msgs[0].(taro.MouseMsg)
			require.True(t, ok,
				"row %d: should be MouseMsg", row)
			require.Equal(t, row-innerTop, mouse.R,
				"row %d: should be translated to inner coordinates", row)
		}
		l.Kill()
	})
}
