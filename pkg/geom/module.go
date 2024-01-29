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

func (v Vec2) IsZero() bool {
	return v.R == 0 && v.C == 0
}

func (v Vec2) Add(other Vec2) Vec2 {
	return Vec2{
		R: other.R + v.R,
		C: other.C + v.C,
	}
}

// Calculate the position that centers the rectangle described by `other`
// inside of the rectangle described by `v`.
func (v Vec2) Center(other Vec2) Vec2 {
	return Vec2{
		R: (v.R / 2) - (other.R / 2),
		C: (v.C / 2) - (other.C / 2),
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
	Position, Size Vec2
}

func (r Rect) Contains(v Vec2) bool {
	return v.R >= r.Position.R && v.R < r.Position.R+r.Size.R && v.C >= r.Position.C && v.C < r.Position.C+r.Size.C
}

// Get the position of the bottom-right corner of the Rect.
func (r Rect) BottomRight() Vec2 {
	return Vec2{
		R: r.Position.R + r.Size.R,
		C: r.Position.C + r.Size.C,
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
