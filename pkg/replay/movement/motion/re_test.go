package motion

import (
	"regexp"
	"testing"

	"github.com/cfoust/cy/pkg/emu"

	"github.com/stretchr/testify/require"
)

func makePattern(pattern string) *regexp.Regexp {
	compiled, err := regexp.Compile(pattern)
	if err != nil {
		panic(err)
	}
	return compiled
}

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
}
