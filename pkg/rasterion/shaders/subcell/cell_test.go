package subcell

import (
	"testing"

	gl "github.com/go-gl/mathgl/mgl32"
	"github.com/stretchr/testify/require"
)

func TestCells(t *testing.T) {
	var f gridCell
	f = f.Set(0, 0, true)
	require.True(t, f.Get(0, 0))
	f = f.Draw(gl.Vec2{}, gl.Vec2{1, 0})
	require.True(t, f.Get(0, 7))
	f = f.Draw(gl.Vec2{}, gl.Vec2{1, 1})
	require.True(t, f.Get(7, 7))
	require.False(t, f.Get(7, 0))
}
