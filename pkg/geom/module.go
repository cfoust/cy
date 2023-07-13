package geom

type Size struct {
	Rows    int
	Columns int
}

var DEFAULT_SIZE = Size{
	Rows:    26,
	Columns: 80,
}

func min(a, b int) int {
	if a < b {
		return a
	}

	return b
}

// Find the largest rectangle that both rectangle `a` and `b` can fit.
func GetMaximum(a, b Size) Size {
	return Size{
		Rows:    min(a.Rows, b.Rows),
		Columns: min(a.Columns, b.Columns),
	}
}
