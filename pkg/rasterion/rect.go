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

// Contains reports whether a Rect contains the given point.
func (r Rect) Contains(other gl.Vec2) bool {
	var (
		rMin = r.Pos
		rMax = r.Pos.Add(r.Size)
	)

	return other.X() >= rMin.X() &&
		other.X() <= rMax.X() &&
		other.Y() >= rMin.Y() &&
		other.Y() <= rMax.Y()
}

// IntersectLine calculates the intersections of two lines, each of which is
// defined by two points. `intersects` is true if and only if the intersection
// of the two lines falls on those lines. In other words, the lines are treated
// as being of finite length.
func IntersectLine(p0, p1, p2, p3 gl.Vec2) (
	intersection gl.Vec2,
	intersects bool,
) {
	denominator := (p0.X()-p1.X())*(p2.Y()-p3.Y()) - (p0.Y()-p1.Y())*(p2.X()-p3.X())
	if denominator == 0 {
		// Lines coincide or are parallel
		return
	}

	// Courtesy of Wikipedia
	intersection[0] = (p0.X()*p1.Y()-p0.Y()*p1.X())*(p2.X()-p3.X()) - (p0.X()-p1.X())*(p2.X()*p3.Y()-p2.Y()*p3.X())
	intersection[0] /= denominator

	intersection[1] = (p0.X()*p1.Y()-p0.Y()*p1.X())*(p2.Y()-p3.Y()) - (p0.Y()-p1.Y())*(p2.X()*p3.Y()-p2.Y()*p3.X())
	intersection[1] /= denominator

	// We only want to return the intersection if it actually occurs
	// between the points defining each line.
	var (
		ray0 = p1.Sub(p0)
		ray1 = p3.Sub(p2)
		ray2 = intersection.Sub(p0)
		ray3 = intersection.Sub(p2)
		mag0 = ray0.Len()
		mag1 = ray1.Len()
		on0  = ray2.Len() <= mag0 && ray2.Dot(ray0) >= 0
		on1  = ray3.Len() <= mag1 && ray3.Dot(ray1) >= 0
	)

	// Only return true if the intersection is on both lines
	intersects = on0 && on1
	return
}

var intersectionChecks = [][2]int{
	// LR, TR, RB, BL, LT, TB
	{3, 1}, {0, 1}, {1, 2}, {2, 3}, {0, 2},
}

func (r Rect) Intersections(p0, p1 gl.Vec2) (
	i0, i1 gl.Vec2,
	intersects bool,
	numIntersections int,
) {
	// No intersections if line inside, but still "intersects" rect
	if r.Contains(p0) && r.Contains(p1) {
		intersects = true
		i0 = p0
		i1 = p1
		return
	}

	var (
		rMin              = r.Pos
		rMax              = r.Pos.Add(r.Size)
		ray               = p1.Sub(p0)
		TL                = rMin
		TR                = gl.Vec2{rMax[0], rMin[1]}
		BL                = gl.Vec2{rMin[0], rMax[1]}
		BR                = rMax
		top, hasTop       = IntersectLine(p0, p1, TL, TR)
		left, hasLeft     = IntersectLine(p0, p1, BL, TL)
		right, hasRight   = IntersectLine(p0, p1, BR, TR)
		bottom, hasBottom = IntersectLine(p0, p1, BL, BR)
		intersections     = []gl.Vec2{top, right, bottom, left}
		has               = []bool{
			hasTop,
			hasRight,
			hasBottom,
			hasLeft,
		}
		has0, has1 bool
	)

	for _, check := range intersectionChecks {
		i0 = intersections[check[0]]
		i1 = intersections[check[1]]
		has0 = has[check[0]]
		has1 = has[check[1]]

		// The line does not intersect these
		if !has0 && !has1 {
			continue
		}

		// It intersects both
		if has0 && has1 {
			// Otherwise we have both
			intersects = true
			numIntersections = 2

			// If original line has opposite direction, swap
			if i1.Sub(i0).Dot(ray) < 0 {
				i0, i1 = i1, i0
			}
			return
		}

		// It must just intersect one, but if neither point is inside
		// the rect, it can't be this pair of sides
		if !r.Contains(p0) && !r.Contains(p1) {
			continue
		}

		intersects = true
		numIntersections = 1

		// Store the intersection in i1
		if has0 {
			i1 = i0
		}

		// Store the inner point in i0
		i0 = p0
		if r.Contains(p1) {
			i0 = p1
		}

		// If original line has opposite direction, swap
		if i1.Sub(i0).Dot(ray) < 0 {
			i0, i1 = i1, i0
		}
		return
	}

	return
}
