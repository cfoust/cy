package splash

import (
	"testing"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/taro"
)

// A client attaching from a pty with no size set used to panic in
// rand.Intn with a zero argument and take the whole server down.
func TestGenerateBackgroundZeroSize(t *testing.T) {
	render := taro.NewRenderer()
	for _, size := range []geom.Size{
		{R: 0, C: 0},
		{R: 0, C: 10},
		{R: 10, C: 0},
		{R: 24, C: 80},
	} {
		img := generateBackground(render, size)
		if img == nil {
			t.Fatalf("nil image for size %v", size)
		}
	}
}
