package geom

type Vec2 struct {
	_ struct{} `janet:"tuple"`
	// row (y)
	R int
	// column (x)
	C int
}

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
