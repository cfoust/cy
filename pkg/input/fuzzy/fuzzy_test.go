package fuzzy

import (
	"context"
	"testing"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/stretchr/testify/require"
)

var smallOptions []Option = []Option{
	NewOption("foo", 0),
	NewOption("bar", 1),
	NewOption("baz", 2),
}

var simpleOptions []Option = []Option{
	NewOption("foo", 0),
	NewOption("bar", 1),
	NewOption("baz", 2),
	NewOption("baz", 2),
	NewOption("baz", 2),
	NewOption("baz", 2),
	NewOption("baz", 2),
	NewOption("baz", 2),
	NewOption("baz", 2),
}

func TestCapitalization(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	f := newFuzzy(ctx, []Option{
		NewOption("foo", 0),
		NewOption("bar", 1),
		NewOption("Baz", 2),
	})
	test := taro.Test(f)
	test("Baz")
	option := f.getOptions()[f.selected]
	require.Equal(t, "Baz", option.Text)
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

func TestWrap(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Wrapping is on by default
	f := newFuzzy(ctx, smallOptions)
	test := taro.Test(f)

	// In isUp mode (default), "up" increases index, "down" decreases
	// Navigate up past the last item, should wrap to first
	test("up", "up", "up")
	require.Equal(t, 0, f.selected)

	// Navigate down from first item, should wrap to last
	test("down")
	require.Equal(t, 2, f.selected)

	// Disable wrapping
	f.params.SetInputFindWrap(false)
	f.selected = 0

	// Navigate down from first item, should clamp to 0
	test("down")
	require.Equal(t, 0, f.selected)

	// Navigate to end, then past it, should clamp
	test("up", "up", "up")
	require.Equal(t, 2, f.selected)
}

func TestPaging(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	f := newFuzzy(ctx, simpleOptions)
	test := taro.Test(f)
	test(geom.Size{
		R: 7, // 5 rows of options
		C: 20,
	})
	test("home")
	require.Equal(t, 8, f.selected)
	test("end")
	require.Equal(t, 0, f.selected)
	f.isUp = false
	test("home")
	require.Equal(t, 0, f.selected)
	test("end")
	require.Equal(t, 8, f.selected)
	f.isUp = true

	test("home", "pgdown")
	require.Equal(t, 3, f.selected)
	test("pgdown")
	require.Equal(t, 0, f.selected)

	f.isUp = false
	test("home", "pgdown")
	require.Equal(t, 5, f.selected)
	test("pgdown")
	require.Equal(t, 8, f.selected)
}
