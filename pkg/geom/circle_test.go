package geom

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func collectSpans(a, b Vec2) []struct{ row, left, right int } {
	var spans []struct{ row, left, right int }
	FilledCircleSpans(a, b, func(row, left, right int) {
		spans = append(spans, struct{ row, left, right int }{row, left, right})
	})
	return spans
}

func spanToGrid(a, b Vec2) []string {
	minR := Min(a.R, b.R)
	maxR := Max(a.R, b.R)
	minC := Min(a.C, b.C)
	maxC := Max(a.C, b.C)

	height := maxR - minR + 1
	width := maxC - minC + 1

	grid := make([][]byte, height)
	for i := range grid {
		grid[i] = make([]byte, width)
		for j := range grid[i] {
			grid[i][j] = '0'
		}
	}

	FilledCircleSpans(a, b, func(row, left, right int) {
		for c := left; c <= right; c++ {
			r := row - minR
			col := c - minC
			if r >= 0 && r < height && col >= 0 && col < width {
				grid[r][col] = '1'
			}
		}
	})

	result := make([]string, height)
	for i, row := range grid {
		result[i] = string(row)
	}
	return result
}

func TestFilledCircle1x1(t *testing.T) {
	spans := collectSpans(Vec2{R: 5, C: 5}, Vec2{R: 5, C: 5})
	require.Len(t, spans, 1)
	require.Equal(t, 5, spans[0].row)
	require.Equal(t, 5, spans[0].left)
	require.Equal(t, 5, spans[0].right)
}

func TestFilledCircle3x3(t *testing.T) {
	grid := spanToGrid(Vec2{R: 0, C: 0}, Vec2{R: 2, C: 2})
	require.Equal(t, []string{
		"010",
		"111",
		"010",
	}, grid)
}

func TestFilledCircle5x5(t *testing.T) {
	grid := spanToGrid(Vec2{R: 0, C: 0}, Vec2{R: 4, C: 4})
	require.Equal(t, []string{
		"01110",
		"11111",
		"11111",
		"11111",
		"01110",
	}, grid)
}

func TestFilledCircleSymmetry(t *testing.T) {
	// The circle should be symmetric horizontally and vertically
	a := Vec2{R: 0, C: 0}
	b := Vec2{R: 6, C: 6}
	grid := spanToGrid(a, b)

	height := len(grid)
	width := len(grid[0])

	// Vertical symmetry
	for r := 0; r < height/2; r++ {
		require.Equal(t, grid[r], grid[height-1-r],
			"row %d should mirror row %d", r, height-1-r)
	}

	// Horizontal symmetry
	for r := 0; r < height; r++ {
		for c := 0; c < width/2; c++ {
			require.Equal(t, grid[r][c], grid[r][width-1-c],
				"cell [%d,%d] should mirror [%d,%d]",
				r, c, r, width-1-c)
		}
	}
}

func TestFilledCircleRectangularBox(t *testing.T) {
	// Wide rectangle: ellipse fills the full 3x7 bounding box
	grid := spanToGrid(Vec2{R: 0, C: 0}, Vec2{R: 2, C: 6})
	require.Equal(t, []string{
		"0111110",
		"1111111",
		"0111110",
	}, grid)
}

func TestFilledCircleTallBox(t *testing.T) {
	// Tall rectangle: ellipse fills the full 7x3 bounding box
	grid := spanToGrid(Vec2{R: 0, C: 0}, Vec2{R: 6, C: 2})
	require.Equal(t, []string{
		"010",
		"111",
		"111",
		"111",
		"111",
		"111",
		"010",
	}, grid)
}

func TestFilledCircleInvertedCorners(t *testing.T) {
	// The function should handle corners in any order
	grid1 := spanToGrid(Vec2{R: 0, C: 0}, Vec2{R: 4, C: 4})
	grid2 := spanToGrid(Vec2{R: 4, C: 4}, Vec2{R: 0, C: 0})
	require.Equal(t, grid1, grid2)

	grid3 := spanToGrid(Vec2{R: 4, C: 0}, Vec2{R: 0, C: 4})
	require.Equal(t, grid1, grid3)
}

func TestFilledCircleCornersExcluded(t *testing.T) {
	// For a 5x5 circle, corners should not be filled
	grid := spanToGrid(Vec2{R: 0, C: 0}, Vec2{R: 4, C: 4})
	require.Equal(t, byte('0'), grid[0][0])
	require.Equal(t, byte('0'), grid[0][4])
	require.Equal(t, byte('0'), grid[4][0])
	require.Equal(t, byte('0'), grid[4][4])
}
