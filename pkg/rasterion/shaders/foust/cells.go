package foust

import (
	"github.com/cfoust/cy/pkg/geom"

	gl "github.com/go-gl/mathgl/mgl32"
)

type foustCell uint64

func getBitOffset(row, col int) int {
	row = geom.Clamp(row, 0, 7)
	col = geom.Clamp(col, 0, 7)
	return ((row * 8) + col)
}

func (c foustCell) Set(row, col int, value bool) foustCell {
	var bit uint64
	if value {
		bit = 1
	}

	offset := getBitOffset(row, col)
	return foustCell(uint64(c) | (bit << offset))
}

func (c foustCell) Get(row, col int) bool {
	offset := getBitOffset(row, col)
	return ((uint64(c) >> offset) & uint64(1)) == 1
}

func (c foustCell) String() (result string) {
	for row := 0; row < foustSize; row++ {
		for col := 0; col < foustSize; col++ {
			if c.Get(row, col) {
				result += "1"
			} else {
				result += "0"
			}
		}

		if row < foustSize-1 {
			result += "\n"
		}
	}

	return
}

const foustSize = 8

func uvToCoord(value float32) int {
	return geom.Clamp(int(value*foustSize), 0, foustSize-1)
}

func (c foustCell) Draw(v0, v1 gl.Vec2) (value foustCell) {
	value = c

	var (
		x0 = uvToCoord(v0[0])
		y0 = uvToCoord(v0[1])
		x1 = uvToCoord(v1[0])
		y1 = uvToCoord(v1[1])
	)

	size := geom.Vec2{
		R: foustSize,
		C: foustSize,
	}
	dx := geom.Abs(x1 - x0)
	dy := geom.Abs(y1 - y0)

	sx := -1
	if x0 < x1 {
		sx = 1
	}
	sy := -1
	if y0 < y1 {
		sy = 1
	}
	err := dx - dy

	for {
		if x0 >= 0 && x0 < size.C && y0 >= 0 && y0 < size.R {
			value = value.Set(y0, x0, true)
		}
		if x0 == x1 && y0 == y1 {
			break
		}
		e2 := 2 * err
		if e2 > -dy {
			err -= dy
			x0 += sx
		}
		if e2 < dx {
			err += dx
			y0 += sy
		}
	}

	return value
}

func (c foustCell) Query() (r rune) {
	r = ' '

	var (
		minIndex    int    = -1
		minCount    uint64 = 64
		diff, count uint64
		value       = uint64(c)
	)

	for i, s := range foustSymbols {
		diff = value ^ uint64(s.Cell)
		count = 0
		for j := 0; j < 64; j++ {
			if (diff & (1 << j)) > 0 {
				count++
			}
		}

		if count < minCount {
			minCount = count
			minIndex = i
		}
	}

	if minIndex != -1 {
		return foustSymbols[minIndex].Rune
	}

	return
}
