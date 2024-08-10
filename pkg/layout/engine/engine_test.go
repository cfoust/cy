package engine

import (
	"context"
	"os"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/layout/pane"
	S "github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	T "github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/stretchr/testify/require"
)

func TestSet(t *testing.T) {
	l := New(
		context.Background(),
		T.NewTree(),
		server.New(),
	)

	err := l.Set(L.New(
		L.MarginsType{
			Node: L.SplitType{
				A: L.PaneType{Attached: true},
				B: L.PaneType{},
			},
		},
	))
	require.NoError(t, err)

	before := l.existing
	err = l.Set(L.New(
		L.MarginsType{
			Node: L.SplitType{
				A: L.PaneType{Attached: true},
				B: L.SplitType{
					A: L.PaneType{},
					B: L.PaneType{},
				},
			},
		},
	))
	require.NoError(t, err)
	require.NotEqual(t, before, l.existing)
}

func TestClickInactivePane(t *testing.T) {
	size := geom.DEFAULT_SIZE
	l := New(
		context.Background(),
		T.NewTree(),
		server.New(),
	)
	l.Resize(size)

	err := l.Set(L.New(
		L.SplitType{
			A: L.PaneType{Attached: true},
			B: L.PaneType{},
		},
	))
	require.NoError(t, err)

	l.Send(taro.MouseMsg{
		Vec2: geom.Vec2{
			R: 0,
			C: 45,
		},
		Type:   taro.MousePress,
		Button: taro.MouseLeft,
	})
	time.Sleep(500 * time.Millisecond)

	require.Equal(t, L.SplitType{
		A: L.PaneType{},
		B: L.PaneType{Attached: true},
	}, l.Get().Root)
}

func TestRemoveAttached(t *testing.T) {
	require.Equal(t,
		L.MarginsType{Node: L.PaneType{Attached: true}},
		L.RemoveAttached(L.SplitType{
			A: L.MarginsType{Node: L.PaneType{}},
			B: L.PaneType{Attached: true},
		}),
	)
}

func TestAttachFirst(t *testing.T) {
	require.Equal(t,
		L.SplitType{
			A: L.MarginsType{Node: L.PaneType{Attached: true}},
			B: L.PaneType{},
		},
		L.AttachFirst(L.SplitType{
			A: L.MarginsType{Node: L.PaneType{}},
			B: L.PaneType{},
		}),
	)
}

func TestPaneRemoval(t *testing.T) {
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
	l.Resize(size)

	createPane := func() (*taro.Program, *T.Pane, *T.NodeID) {
		static := pane.NewStatic(
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
		_, pane1, id1 := createPane()
		_, pane2, id2 := createPane()

		removeOnExit := true
		err := l.Set(L.New(
			L.SplitType{
				A: L.PaneType{
					Attached: true,
					ID:       id1,
				},
				B: L.PaneType{
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
		require.Equal(t, L.SplitType{
			A: L.PaneType{
				Attached: true,
				ID:       nil,
			},
			B: L.PaneType{
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}, l.Get().Root)

		// Switch attachment to B
		require.NoError(t, l.Set(L.New(
			L.SplitType{
				A: L.PaneType{
					ID: nil,
				},
				B: L.PaneType{
					Attached:     true,
					ID:           id2,
					RemoveOnExit: &removeOnExit,
				},
			},
		)))

		// This pane should not stick around
		pane2.Cancel()
		sleep()
		require.Equal(t, L.PaneType{
			Attached: true,
			ID:       nil,
		}, l.Get().Root)
	}

	// Next we test exiting the panes themselves
	{
		static1, _, id1 := createPane()
		static2, _, id2 := createPane()

		removeOnExit := true
		layout := L.SplitType{
			A: L.PaneType{
				Attached: true,
				ID:       id1,
			},
			B: L.PaneType{
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}

		require.NoError(t, l.Set(L.New(layout)))

		// If static1 exits without an error and removeOnExit is not
		// true, nothing should happen.
		static1.Publish(S.ExitEvent{})
		sleep()
		require.Equal(t, L.SplitType{
			A: L.PaneType{
				Attached: true,
				ID:       id1,
			},
			B: L.PaneType{
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}, l.Get().Root)

		// Reset the layout to force new subscriptions
		require.NoError(t, l.Set(L.New(L.PaneType{Attached: true})))
		require.NoError(t, l.Set(L.New(layout)))

		// static1 exiting with an error should still cause nothing to happen
		static1.Publish(S.ExitEvent{
			Errored: true,
		})
		sleep()
		require.Equal(t, L.SplitType{
			A: L.PaneType{
				Attached: true,
				ID:       id1,
			},
			B: L.PaneType{
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}, l.Get().Root)

		layout = L.SplitType{
			A: L.PaneType{
				ID: id1,
			},
			B: L.PaneType{
				Attached:     true,
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}
		require.NoError(t, l.Set(L.New(L.PaneType{Attached: true})))
		require.NoError(t, l.Set(L.New(layout)))
		sleep()

		// If static2 exits without an error, it should remove the node
		static2.Publish(S.ExitEvent{})
		sleep()
		require.Equal(t, L.PaneType{
			Attached: true,
			ID:       id1,
		}, l.Get().Root)

		// Reset
		require.NoError(t, l.Set(L.New(L.PaneType{Attached: true})))
		require.NoError(t, l.Set(L.New(layout)))

		// If static2 exits with an error, it should NOT remove the node
		static2.Publish(S.ExitEvent{Errored: true})
		sleep()
		require.Equal(t, layout, l.Get().Root)
	}
}

func TestClickTabs(t *testing.T) {
	size := geom.DEFAULT_SIZE
	l := New(
		context.Background(),
		T.NewTree(),
		server.New(),
	)
	l.Resize(size)

	name := "tab"
	err := l.Set(L.New(
		L.TabsType{
			Tabs: []L.Tab{
				{
					Name:   name,
					Active: true,
					Node:   L.PaneType{Attached: true},
				},
				{
					Name:   name,
					Active: false,
					Node:   L.PaneType{},
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
			Type:   taro.MousePress,
			Button: taro.MouseLeft,
		})
		time.Sleep(500 * time.Millisecond)
	}

	// Should be on the second tab
	click(geom.Vec2{C: 5})
	require.Equal(t, L.TabsType{
		Tabs: []L.Tab{
			{
				Name:   name,
				Active: false,
				Node:   L.PaneType{},
			},
			{
				Name:   name,
				Active: true,
				Node:   L.PaneType{Attached: true},
			},
		},
	}, l.Get().Root)

	// Should be on the first tab
	click(geom.Vec2{C: 1})
	require.Equal(t, L.TabsType{
		Tabs: []L.Tab{
			{
				Name:   name,
				Active: true,
				Node:   L.PaneType{Attached: true},
			},
			{
				Name:   name,
				Active: false,
				Node:   L.PaneType{},
			},
		},
	}, l.Get().Root)

	// This should not cause any trouble
	click(geom.Vec2{C: 1})
	require.Equal(t, L.TabsType{
		Tabs: []L.Tab{
			{
				Name:   name,
				Active: true,
				Node:   L.PaneType{Attached: true},
			},
			{
				Name:   name,
				Active: false,
				Node:   L.PaneType{},
			},
		},
	}, l.Get().Root)

	// Now set nested tabs
	err = l.Set(L.New(
		L.TabsType{
			Tabs: []L.Tab{
				{
					Name:   name,
					Active: true,
					Node:   L.PaneType{Attached: true},
				},
				{
					Name:   name,
					Active: false,
					Node: L.TabsType{
						Tabs: []L.Tab{
							{
								Name:   name,
								Active: true,
								Node:   L.PaneType{},
							},
						},
					},
				},
			},
		},
	))
	require.NoError(t, err)
	click(geom.Vec2{C: 5})

	require.Equal(t, L.TabsType{
		Tabs: []L.Tab{
			{
				Name:   name,
				Active: false,
				Node:   L.PaneType{},
			},
			{
				Name:   name,
				Active: true,
				Node: L.TabsType{
					Tabs: []L.Tab{
						{
							Name:   name,
							Active: true,
							Node:   L.PaneType{Attached: true},
						},
					},
				},
			},
		},
	}, l.Get().Root)
}
