package layout

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestValidate(t *testing.T) {
	require.Error(t, ValidateTree(&SplitNode{
		A: &PaneNode{
			Attached: true,
		},
		B: &PaneNode{
			Attached: true,
		},
	}))
	require.Error(t, ValidateTree(&SplitNode{
		A: &PaneNode{},
		B: &PaneNode{},
	}))
	require.NoError(t, ValidateTree(&SplitNode{
		A: &PaneNode{
			Attached: true,
		},
		B: &PaneNode{},
	}))
	require.Error(t, ValidateTree(&TabsNode{
		Tabs: []Tab{},
	}))
	require.Error(t, ValidateTree(&TabsNode{
		Tabs: []Tab{
			{},
		},
	}))
	require.NoError(t, ValidateTree(&TabsNode{
		Tabs: []Tab{
			{
				Name:   "foo",
				Active: true,
				Node: &PaneNode{
					Attached: true,
				},
			},
		},
	}))
	require.Error(t, ValidateTree(&TabsNode{
		Tabs: []Tab{
			{
				Name:   "foo",
				Active: true,
				// The inner node has no active tab
				Node: &TabsNode{
					Tabs: []Tab{
						{
							Active: false,
							Node: &PaneNode{
								Attached: true,
							},
						},
					},
				},
			},
		},
	}))
}

func TestAttachFirst(t *testing.T) {
	require.Equal(t, &TabsNode{
		Tabs: []Tab{
			{
				Name:   "foo",
				Active: true,
				Node: &PaneNode{
					Attached: true,
				},
			},
		},
	}, AttachFirst(&TabsNode{
		Tabs: []Tab{
			{
				Name:   "foo",
				Active: true,
				Node: &PaneNode{
					Attached: false,
				},
			},
		},
	}))

	require.Equal(t,
		&SplitNode{
			A: &MarginsNode{Node: &PaneNode{Attached: true}},
			B: &PaneNode{},
		},
		AttachFirst(&SplitNode{
			A: &MarginsNode{Node: &PaneNode{}},
			B: &PaneNode{},
		}),
	)
}

func TestRemoveAttached(t *testing.T) {
	require.Equal(t,
		&MarginsNode{Node: &PaneNode{Attached: true}},
		RemoveAttached(&SplitNode{
			A: &MarginsNode{Node: &PaneNode{}},
			B: &PaneNode{Attached: true},
		}),
	)
}
