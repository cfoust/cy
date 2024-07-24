package layout

import (
	"testing"

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
