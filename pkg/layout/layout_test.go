package layout

import (
	"context"
	"testing"

	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"

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
