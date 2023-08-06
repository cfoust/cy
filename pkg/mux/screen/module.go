package screen

import (
	"github.com/cfoust/cy/pkg/mux"

	"github.com/muesli/termenv"
	"github.com/xo/terminfo"
)

type Screen = mux.Screen
type Stream = mux.Stream
type Updater = mux.Updater
type Size = mux.Size

// RenderContext contains information about the rendering environment, such as
// the supported color mode and terminfo database.
type RenderContext struct {
	*terminfo.Terminfo
	Colors termenv.Profile
}
