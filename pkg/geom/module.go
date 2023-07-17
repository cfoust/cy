package geom

type Size struct {
	Rows    int
	Columns int
}

type Vec4 struct {
	R int
	C int
	W int
	H int
}

var DEFAULT_SIZE = Size{
	Rows:    26,
	Columns: 80,
}

func Min(a, b int) int {
	if a < b {
		return a
	}

	return b
}

// Find the largest rectangle that both rectangle `a` and `b` can fit.
func GetMaximum(a, b Size) Size {
	return Size{
		Rows:    Min(a.Rows, b.Rows),
		Columns: Min(a.Columns, b.Columns),
	}
}
