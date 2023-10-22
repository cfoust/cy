package geom

type Vec2 struct {
	_ struct{} `janet:"tuple"`
	// row (y)
	R int
	// column (x)
	C int
}

func (v Vec2) Scalar(value int) Vec2 {
	return Vec2{
		R: v.R * value,
		C: v.C * value,
	}
}

func (v Vec2) Sub(other Vec2) Vec2 {
	return v.Add(other.Scalar(-1))
}

func (v Vec2) Add(other Vec2) Vec2 {
	return Vec2{
		R: other.R + v.R,
		C: other.C + v.C,
	}
}

func (v Vec2) Clamp(min, max Vec2) Vec2 {
	return Vec2{
		R: Clamp(v.R, min.R, max.R),
		C: Clamp(v.C, min.C, max.C),
	}
}

var UnitVec2 = Vec2{R: 1, C: 1}

type Size = Vec2

type Rect struct {
	// row (y)
	R int
	// column (x)
	C int
	// height (y)
	H int
	// width (x)
	W int
}

func (r Rect) Size() Vec2 {
	return Vec2{
		R: r.H,
		C: r.W,
	}
}

func (r Rect) Position() Vec2 {
	return Vec2{
		R: r.R,
		C: r.C,
	}
}

var DEFAULT_SIZE = Vec2{
	R: 26,
	C: 80,
}

// Find the largest rectangle that both rectangle `a` and `b` can fit.
func GetMaximum(a, b Vec2) Vec2 {
	return Vec2{
		R: Min(a.R, b.R),
		C: Min(a.C, b.C),
	}
}
