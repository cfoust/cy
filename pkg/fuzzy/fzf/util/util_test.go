package util

import (
	"math"
	"testing"
)

func TestMax16(t *testing.T) {
	if Max16(10, 1) != 10 {
		t.Error("Expected", 10)
	}
	if Max16(-2, 5) != 5 {
		t.Error("Expected", 5)
	}
	if Max16(math.MaxInt16, 0) != math.MaxInt16 {
		t.Error("Expected", math.MaxInt16)
	}
	if Max16(0, math.MinInt16) != 0 {
		t.Error("Expected", 0)
	}
}

func TestMax32(t *testing.T) {
	if Max32(10, 1) != 10 {
		t.Error("Expected", 10)
	}
	if Max32(-2, 5) != 5 {
		t.Error("Expected", 5)
	}
	if Max32(math.MaxInt32, 0) != math.MaxInt32 {
		t.Error("Expected", math.MaxInt32)
	}
	if Max32(0, math.MinInt32) != 0 {
		t.Error("Expected", 0)
	}
}

func TestMin(t *testing.T) {
	if Min(10, 1) != 1 {
		t.Error("Expected", 1)
	}
	if Min(-2, 5) != -2 {
		t.Error("Expected", -2)
	}
}

func TestAsUint16(t *testing.T) {
	if AsUint16(5) != 5 {
		t.Error("Expected", 5)
	}
	if AsUint16(-10) != 0 {
		t.Error("Expected", 0)
	}
	if AsUint16(math.MaxUint16) != math.MaxUint16 {
		t.Error("Expected", math.MaxUint16)
	}
	if AsUint16(math.MinInt32) != 0 {
		t.Error("Expected", 0)
	}
	if AsUint16(math.MinInt16) != 0 {
		t.Error("Expected", 0)
	}
	if AsUint16(math.MaxUint16+1) != math.MaxUint16 {
		t.Error("Expected", math.MaxUint16)
	}
}
