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
}
