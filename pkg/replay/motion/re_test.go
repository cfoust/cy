package motion

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"

	"github.com/stretchr/testify/require"
)

func TestFind(t *testing.T) {
	require.Equal(
		t,
		[]int{3, 6},
		findLine(
			makePattern("bar"),
			emu.LineFromString("foobar"),
		),
	)
	require.Equal(
		t,
		[]int{4, 7},
		findLine(
			makePattern("bar"),
			emu.LineFromString("你好bar"),
		),
	)
	require.Equal(
		t,
		[][]int{
			{0, 3},
			{4, 7},
			{8, 11},
		},
		findAllLine(
			makePattern("foo"),
			emu.LineFromString("foo foo foo"),
		),
	)
}
