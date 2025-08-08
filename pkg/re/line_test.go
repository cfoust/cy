package re

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"

	"github.com/stretchr/testify/require"
)

func TestFindLine(t *testing.T) {
	require.Equal(
		t,
		[]int{3, 6},
		FindLine(
			MakePattern("bar"),
			emu.LineFromString("foobar"),
		),
	)
	require.Equal(
		t,
		[]int{4, 7},
		FindLine(
			MakePattern("bar"),
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
		FindAllLine(
			MakePattern("foo"),
			emu.LineFromString("foo foo foo"),
		),
	)
	require.Equal(
		t,
		([]int)(nil),
		FindLine(
			MakePattern("bar"),
			emu.LineFromString("foo"),
		),
	)
}
