package geom

// FilledCircleSpans calls fn for each row of the filled ellipse inscribed
// in the bounding box defined by corners a and b. The ellipse fills the
// full bounding box. Uses the midpoint ellipse algorithm with integer
// arithmetic only. left and right are inclusive column bounds.
func FilledCircleSpans(a, b Vec2, fn func(row, left, right int)) {
	// Normalize the bounding box
	minR := Min(a.R, b.R)
	maxR := Max(a.R, b.R)
	minC := Min(a.C, b.C)
	maxC := Max(a.C, b.C)

	width := maxC - minC + 1
	height := maxR - minR + 1

	if width <= 0 || height <= 0 {
		return
	}

	centerR := minR + height/2
	centerC := minC + width/2

	rx := (width - 1) / 2
	ry := (height - 1) / 2

	if rx == 0 && ry == 0 {
		fn(centerR, centerC, centerC)
		return
	}

	if ry == 0 {
		fn(centerR, centerC-rx, centerC+rx)
		return
	}

	if rx == 0 {
		for dy := -ry; dy <= ry; dy++ {
			fn(centerR+dy, centerC, centerC)
		}
		return
	}

	// Track the maximum x-extent for each row offset from center.
	maxX := make([]int, ry+1)

	var (
		rx2 = rx * rx
		ry2 = ry * ry
		x   = 0
		y   = ry
	)

	// Midpoint ellipse algorithm, Region 1 (|slope| < 1).
	// Decision parameter scaled by 4 to keep integer arithmetic:
	//   4*d1 = 4*ry² - 4*rx²*ry + rx²
	d := 4*ry2 - 4*rx2*ry + rx2

	for ry2*x <= rx2*y {
		maxX[y] = Max(maxX[y], x)

		if d < 0 {
			d += 4 * ry2 * (2*x + 3)
		} else {
			d += 4*ry2*(2*x+3) + 4*rx2*(-2*y+2)
			y--
		}
		x++
	}

	// Region 2 (|slope| >= 1).
	// Decision parameter scaled by 4:
	//   4*d2 = ry²*(2x+1)² + 4*rx²*(y-1)² - 4*rx²*ry²
	d = ry2*(2*x+1)*(2*x+1) +
		4*rx2*(y-1)*(y-1) -
		4*rx2*ry2

	for y >= 0 {
		maxX[y] = Max(maxX[y], x)

		if d > 0 {
			d += 4 * rx2 * (-2*y + 3)
		} else {
			d += 4*ry2*(2*x+2) + 4*rx2*(-2*y+3)
			x++
		}
		y--
	}

	// Emit spans from top to bottom
	for dy := -ry; dy <= ry; dy++ {
		abs := dy
		if abs < 0 {
			abs = -abs
		}
		dx := maxX[abs]
		fn(centerR+dy, centerC-dx, centerC+dx)
	}
}
