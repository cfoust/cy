package fluid

import (
	"testing"
)

func TestSim(t *testing.T) {
	s := New(100, 100, 100)
	s.Update(1)
}
