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
				Node: &ViewNode{
					Attached: true,
				},
			},
		},
	}, AttachFirst(&TabsNode{
		Tabs: []Tab{
			{
				Name:   "foo",
				Active: true,
				Node: &ViewNode{
					Attached: false,
				},
			},
		},
	}))

	require.Equal(t,
		&SplitNode{
			A: &MarginsNode{Node: &ViewNode{Attached: true}},
			B: &ViewNode{},
		},
		AttachFirst(&SplitNode{
			A: &MarginsNode{Node: &ViewNode{}},
			B: &ViewNode{},
		}),
	)
}

func TestAttachFirstStack(t *testing.T) {
	require.Equal(t, &StackNode{
		Leaves: []Leaf{
			{
				Active: true,
				Node: &ViewNode{
					Attached: true,
				},
			},
		},
	}, AttachFirst(&StackNode{
		Leaves: []Leaf{
			{
				Active: true,
				Node: &ViewNode{
					Attached: false,
				},
			},
		},
	}))
}

func TestRemoveAttached(t *testing.T) {
	require.Equal(t,
		&MarginsNode{Node: &ViewNode{Attached: true}},
		RemoveAttached(&SplitNode{
			A: &MarginsNode{Node: &ViewNode{}},
			B: &ViewNode{Attached: true},
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
					Node:   &ViewNode{Attached: true},
				},
			},
		},
		RemoveAttached(&StackNode{
			Leaves: []Leaf{
				{
					Active: true,
					Node:   &ViewNode{Attached: true},
				},
				{
					Node: &ViewNode{},
				},
			},
		}),
	)

	// With one leaf, removing should unwrap (preserving attached state)
	require.Equal(t,
		&ViewNode{Attached: true},
		RemoveAttached(&StackNode{
			Leaves: []Leaf{
				{
					Active: true,
					Node:   &ViewNode{Attached: true},
				},
			},
		}),
	)
}

func TestAttach(t *testing.T) {
	var id int32 = 1
	node := Attach(&SplitNode{
		A: &ViewNode{Attached: false},
		B: &ViewNode{Attached: true},
	}, id)

	require.Equal(t,
		id,
		*node.Children()[1].(*ViewNode).ID,
	)
}
