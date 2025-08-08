package preview

import (
	"context"

	I "github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"
)

type Static struct {
	image I.Image
}

var _ taro.Model = (*Static)(nil)

func (n *Static) Init() taro.Cmd {
	return nil
}

func (n *Static) Update(msg taro.Msg) (taro.Model, taro.Cmd) {
	return n, nil
}

func (f *Static) View(out *tty.State) {
	out.Image = f.image
}

func NewStatic(ctx context.Context, image I.Image) mux.Screen {
	return taro.New(ctx, &Static{image: image})
}
