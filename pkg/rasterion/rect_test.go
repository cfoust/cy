package rasterion

import (
	"testing"

	gl "github.com/go-gl/mathgl/mgl32"
	"github.com/stretchr/testify/require"
)

func TestLineIntersect(t *testing.T) {
	point, ok := IntersectLine(
		gl.Vec2{-1, 1},
		gl.Vec2{1, 1},
		gl.Vec2{0, 2},
		gl.Vec2{0, -2},
	)
	require.Equal(t, gl.Vec2{0, 1}, point)
	require.True(t, ok)
}

func TestIntersections(t *testing.T) {
	// Unit square centered on origin
	r := Rect{
		Pos:  gl.Vec2{-0.5, -0.5},
		Size: gl.Vec2{1, 1},
	}

	// Completely outside rect
	{
		_, _, ok, _ := r.Intersections(
			gl.Vec2{-1, 0},
			gl.Vec2{-1, -1},
		)
		require.False(t, ok)
	}

	// Completely inside rect
	{
		_, _, ok, num := r.Intersections(
			gl.Vec2{},
			gl.Vec2{},
		)
		require.True(t, ok)
		require.Equal(t, 0, num)
	}

	// Straddles square
	{
		i0, i1, ok, num := r.Intersections(
			gl.Vec2{0, -2},
			gl.Vec2{0, 1},
		)
		require.True(t, ok)
		require.Equal(t, 2, num)
		require.Equal(t, gl.Vec2{0, -0.5}, i0)
		require.Equal(t, gl.Vec2{0, 0.5}, i1)
	}

	// Start point inside
	{
		i0, i1, ok, num := r.Intersections(
			gl.Vec2{0, 0},
			gl.Vec2{0, 1},
		)
		require.True(t, ok)
		require.Equal(t, 1, num)
		require.Equal(t, gl.Vec2{0, 0}, i0)
		require.Equal(t, gl.Vec2{0, 0.5}, i1)
	}

	// Start point inside II
	{
		i0, i1, ok, num := Rect{
			Size: gl.Vec2{1, 1},
		}.Intersections(
			gl.Vec2{0.5, 0.5},
			gl.Vec2{0.5, -1},
		)
		require.True(t, ok)
		require.Equal(t, 1, num)
		require.Equal(t, gl.Vec2{0.5, 0.5}, i0)
		require.Equal(t, gl.Vec2{0.5, 0}, i1)
	}

	// End point inside
	{
		i0, i1, ok, num := r.Intersections(
			gl.Vec2{0, -2},
			gl.Vec2{0, 0},
		)
		require.True(t, ok)
		require.Equal(t, 1, num)
		require.Equal(t, gl.Vec2{0, -0.5}, i0)
		require.Equal(t, gl.Vec2{0, 0}, i1)
	}

	// Drawing bug
	{
		_, _, ok, _ := Rect{
			Pos:  gl.Vec2{81, 23},
			Size: gl.Vec2{1, 1},
		}.Intersections(
			gl.Vec2{80, 24},
			gl.Vec2{83.46251, 21.83644},
		)
		require.True(t, ok)
	}
}
