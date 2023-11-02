package splash

import (
	"fmt"
	"math/rand"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
)

// toilet -f smmono12 cy
const CY_LOGO = `
 ▟██▖▝█ █▌
▐▛  ▘ █▖█
▐▌    ▐█▛
▝█▄▄▌  █▌
 ▝▀▀   █
      █▌
`

func generateBackground(render *taro.Renderer, size geom.Size) image.Image {
	state := image.New(size)

	r := rand.New(rand.NewSource(time.Now().UnixNano()))

	for i := 0; i < 10+r.Intn(25); i++ {
		style := render.
			NewStyle().
			Foreground(lipgloss.Color(
				fmt.Sprintf("%d", r.Intn(256)),
			))
		render.RenderAt(
			state,
			r.Intn(size.R), r.Intn(size.C),
			style.Render(CY_LOGO),
		)
	}

	return state
}
