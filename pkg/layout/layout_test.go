package layout

import (
	"testing"

	"github.com/stretchr/testify/require"
)

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

func TestAttachFirstStack(t *testing.T) {
	require.Equal(t, &StackNode{
		Leaves: []Leaf{
			{
				Active: true,
				Node: &PaneNode{
					Attached: true,
				},
			},
		},
	}, AttachFirst(&StackNode{
		Leaves: []Leaf{
			{
				Active: true,
				Node: &PaneNode{
					Attached: false,
				},
			},
		},
	}))
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

func TestRemoveAttachedStack(t *testing.T) {
	// With two leaves, removing the attached one should return
	// a stack with the remaining leaf (now active and attached)
	require.Equal(t,
		&StackNode{
			Leaves: []Leaf{
				{
					Active: true,
					Node:   &PaneNode{Attached: true},
				},
			},
		},
		RemoveAttached(&StackNode{
			Leaves: []Leaf{
				{
					Active: true,
					Node:   &PaneNode{Attached: true},
				},
				{
					Node: &PaneNode{},
				},
			},
		}),
	)

	// With one leaf, removing should unwrap (preserving attached state)
	require.Equal(t,
		&PaneNode{Attached: true},
		RemoveAttached(&StackNode{
			Leaves: []Leaf{
				{
					Active: true,
					Node:   &PaneNode{Attached: true},
				},
			},
		}),
	)
}

func TestAttach(t *testing.T) {
	var id int32 = 1
	node := Attach(&SplitNode{
		A: &PaneNode{Attached: false},
		B: &PaneNode{Attached: true},
	}, id)

	require.Equal(t,
		id,
		*node.Children()[1].(*PaneNode).ID,
	)
}
