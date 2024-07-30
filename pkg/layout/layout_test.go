package layout

import (
	"context"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
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
		tree.NewTree(),
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
		tree.NewTree(),
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
