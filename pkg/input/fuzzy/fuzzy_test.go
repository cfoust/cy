package fuzzy

import (
	"context"
	"testing"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/stretchr/testify/require"
)

var simpleOptions []Option = []Option{
	NewOption("foo", 0),
	NewOption("bar", 1),
	NewOption("baz", 2),
}

func TestBasic(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	f := newFuzzy(ctx, simpleOptions)
	test := taro.Test(f)
	test("up", "up")
	require.Equal(t, 2, f.selected)
}

func TestSmall(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	f := newFuzzy(ctx, simpleOptions)
	test := taro.Test(f)
	// Should not cause a crash
	test(geom.Size{
		R: 1,
		C: 20,
	})
}
