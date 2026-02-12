package image

import (
	"testing"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/require"
)

func TestZeroSizeImage(t *testing.T) {
	var zero Image

	// Should not panic on zero-size images
	require.Equal(t, geom.Vec2{}, zero.Size())

	require.NotPanics(t, func() {
		zero.Clear(geom.Rect{
			Position: geom.Vec2{},
			Size:     geom.Vec2{R: 10, C: 10},
		})
	})

	require.NotPanics(t, func() {
		dst := New(geom.Vec2{R: 5, C: 5})
		Copy(geom.Vec2{}, dst, zero)
	})

	require.NotPanics(t, func() {
		dst := New(geom.Vec2{R: 5, C: 5})
		CopyRaw(geom.Vec2{}, dst, zero)
	})

	require.NotPanics(t, func() {
		dst := New(geom.Vec2{R: 5, C: 5})
		Compose(geom.Vec2{}, dst, zero)
	})

	require.NotPanics(t, func() {
		Copy(geom.Vec2{}, zero, New(geom.Vec2{R: 5, C: 5}))
	})

	require.NotPanics(t, func() {
		cloned := zero.Clone()
		require.Equal(t, geom.Vec2{}, cloned.Size())
	})
}
