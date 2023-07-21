package util

import (
	"math"
)

// Max returns the largest integer
func Max(first int, second int) int {
	if first >= second {
		return first
	}
	return second
}

// Max16 returns the largest integer
func Max16(first int16, second int16) int16 {
	if first >= second {
		return first
	}
	return second
}

// Max32 returns the largest 32-bit integer
func Max32(first int32, second int32) int32 {
	if first > second {
		return first
	}
	return second
}

// Min returns the smallest integer
func Min(first int, second int) int {
	if first <= second {
		return first
	}
	return second
}

func AsUint16(val int) uint16 {
	if val > math.MaxUint16 {
		return math.MaxUint16
	} else if val < 0 {
		return 0
	}
	return uint16(val)
}
