package layout

import (
	"context"
	"os"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	S "github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	T "github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/stretchr/testify/require"
)

func TestValidate(t *testing.T) {
	require.Error(t, validateTree(SplitType{
		A: PaneType{
			Attached: true,
		},
		B: PaneType{
			Attached: true,
		},
	}))
	require.Error(t, validateTree(SplitType{
		A: PaneType{},
		B: PaneType{},
	}))
	require.NoError(t, validateTree(SplitType{
		A: PaneType{
			Attached: true,
		},
		B: PaneType{},
	}))
}

func TestSet(t *testing.T) {
	l := NewLayoutEngine(
		context.Background(),
		T.NewTree(),
		server.New(),
	)

	err := l.Set(New(
		MarginsType{
			Node: SplitType{
				A: PaneType{Attached: true},
				B: PaneType{},
			},
		},
	))
	require.NoError(t, err)

	before := l.existing
	err = l.Set(New(
		MarginsType{
			Node: SplitType{
				A: PaneType{Attached: true},
				B: SplitType{
					A: PaneType{},
					B: PaneType{},
				},
			},
		},
	))
	require.NoError(t, err)
	require.NotEqual(t, before, l.existing)
}

func TestClickInactivePane(t *testing.T) {
	size := geom.DEFAULT_SIZE
	l := NewLayoutEngine(
		context.Background(),
		T.NewTree(),
		server.New(),
	)
	l.Resize(size)

	err := l.Set(New(
		SplitType{
			A: PaneType{Attached: true},
			B: PaneType{},
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

	require.Equal(t, SplitType{
		A: PaneType{},
		B: PaneType{Attached: true},
	}, l.Get().root)
}

func TestRemoveAttached(t *testing.T) {
	require.Equal(t,
		MarginsType{Node: PaneType{Attached: true}},
		removeAttached(SplitType{
			A: MarginsType{Node: PaneType{}},
			B: PaneType{Attached: true},
		}),
	)
}

func TestAttachFirst(t *testing.T) {
	require.Equal(t,
		SplitType{
			A: MarginsType{Node: PaneType{Attached: true}},
			B: PaneType{},
		},
		attachFirst(SplitType{
			A: MarginsType{Node: PaneType{}},
			B: PaneType{},
		}),
	)
}

func TestPaneRemoval(t *testing.T) {
	ctx := context.Background()
	size := geom.DEFAULT_SIZE
	tree := T.NewTree()
	params := params.New()
	l := NewLayoutEngine(
		ctx,
		tree,
		server.New(),
		WithParams(params),
	)
	l.Resize(size)

	createPane := func() (*taro.Program, *T.Pane, *T.NodeID) {
		static := NewStatic(
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
		err := l.Set(New(
			SplitType{
				A: PaneType{
					Attached: true,
					ID:       id1,
				},
				B: PaneType{
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
		require.Equal(t, SplitType{
			A: PaneType{
				Attached: true,
				ID:       nil,
			},
			B: PaneType{
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}, l.Get().root)

		// Switch attachment to B
		require.NoError(t, l.Set(New(
			SplitType{
				A: PaneType{
					ID: nil,
				},
				B: PaneType{
					Attached:     true,
					ID:           id2,
					RemoveOnExit: &removeOnExit,
				},
			},
		)))

		// This pane should not stick around
		pane2.Cancel()
		sleep()
		require.Equal(t, PaneType{
			Attached: true,
			ID:       nil,
		}, l.Get().root)
	}

	// Next we test exiting the panes themselves
	{
		static1, _, id1 := createPane()
		static2, _, id2 := createPane()

		removeOnExit := true
		layout := SplitType{
			A: PaneType{
				Attached: true,
				ID:       id1,
			},
			B: PaneType{
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}

		require.NoError(t, l.Set(New(layout)))

		// If static1 exits without an error and removeOnExit is not
		// true, nothing should happen.
		static1.Publish(S.ExitEvent{})
		sleep()
		require.Equal(t, SplitType{
			A: PaneType{
				Attached: true,
				ID:       id1,
			},
			B: PaneType{
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}, l.Get().root)

		// Reset the layout to force new subscriptions
		require.NoError(t, l.Set(New(PaneType{Attached: true})))
		require.NoError(t, l.Set(New(layout)))

		// static1 exiting with an error should still cause nothing to happen
		static1.Publish(S.ExitEvent{
			Errored: true,
		})
		sleep()
		require.Equal(t, SplitType{
			A: PaneType{
				Attached: true,
				ID:       id1,
			},
			B: PaneType{
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}, l.Get().root)

		layout = SplitType{
			A: PaneType{
				ID: id1,
			},
			B: PaneType{
				Attached:     true,
				ID:           id2,
				RemoveOnExit: &removeOnExit,
			},
		}
		require.NoError(t, l.Set(New(PaneType{Attached: true})))
		require.NoError(t, l.Set(New(layout)))
		sleep()

		// If static2 exits without an error, it should remove the node
		static2.Publish(S.ExitEvent{})
		sleep()
		require.Equal(t, PaneType{
			Attached: true,
			ID:       id1,
		}, l.Get().root)

		// Reset
		require.NoError(t, l.Set(New(PaneType{Attached: true})))
		require.NoError(t, l.Set(New(layout)))

		// If static2 exits with an error, it should NOT remove the node
		static2.Publish(S.ExitEvent{Errored: true})
		sleep()
		require.Equal(t, layout, l.Get().root)
	}
}
