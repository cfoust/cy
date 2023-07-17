package geom

type Vec2 struct {
	// row (y)
	R int
	// column (x)
	C int
}

type Vec4 struct {
	// row (y)
	R int
	// column (x)
	C int
	// height (y)
	H int
	// width (x)
	W int
}

var DEFAULT_SIZE = Vec2{
	R: 26,
	C: 80,
}

func Min(a, b int) int {
	if a < b {
		return a
	}

	return b
}

// Find the largest rectangle that both rectangle `a` and `b` can fit.
func GetMaximum(a, b Vec2) Vec2 {
	return Vec2{
		R: Min(a.R, b.R),
		C: Min(a.C, b.C),
	}
}
