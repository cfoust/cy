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

func TestRemoveAttached(t *testing.T) {
	require.Equal(t,
		&MarginsNode{Node: &PaneNode{Attached: true}},
		RemoveAttached(&SplitNode{
			A: &MarginsNode{Node: &PaneNode{}},
			B: &PaneNode{Attached: true},
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
