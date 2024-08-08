package layout

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestValidate(t *testing.T) {
	require.Error(t, ValidateTree(SplitType{
		A: PaneType{
			Attached: true,
		},
		B: PaneType{
			Attached: true,
		},
	}))
	require.Error(t, ValidateTree(SplitType{
		A: PaneType{},
		B: PaneType{},
	}))
	require.NoError(t, ValidateTree(SplitType{
		A: PaneType{
			Attached: true,
		},
		B: PaneType{},
	}))
	require.Error(t, ValidateTree(TabsType{
		Tabs: []Tab{},
	}))
	require.Error(t, ValidateTree(TabsType{
		Tabs: []Tab{
			{},
		},
	}))
	require.NoError(t, ValidateTree(TabsType{
		Tabs: []Tab{
			{
				Active: true,
				Node: PaneType{
					Attached: true,
				},
			},
		},
	}))
}
