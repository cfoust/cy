package rasterion

import (
	gl "github.com/go-gl/mathgl/mgl32"
)

type Rect struct {
	Pos  gl.Vec2
	Size gl.Vec2
}

// Overlaps reports whether two Rects overlap.
func (r Rect) Overlaps(other Rect) bool {
	var (
		rMin = r.Pos
		rMax = r.Pos.Add(r.Size)
		oMin = other.Pos
		oMax = other.Pos.Add(other.Size)
	)

	return rMin.X() < oMax.X() &&
		rMax.X() > oMin.X() &&
		rMin.Y() < oMax.Y() &&
		rMax.Y() > oMin.Y()
}
