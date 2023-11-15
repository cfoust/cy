package frames

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"

	"github.com/sasha-s/go-deadlock"
)

type Framer struct {
	deadlock.RWMutex
	*mux.UpdatePublisher

	frame  Frame
	output image.Image
	size   geom.Size
}

var _ mux.Screen = (*Framer)(nil)

func (f *Framer) Send(msg mux.Msg) {
}

func (f *Framer) State() *tty.State {
	// TODO(cfoust): 11/17/23 double alloc
	state := tty.New(f.size)
	state.Image = f.output
	state.CursorVisible = false
	return state
}

func (f *Framer) regenerate() {
	f.RLock()
	f.output = image.New(f.size)
	f.frame(f.output)
	f.RUnlock()
	f.Notify()
}

func (f *Framer) Resize(size geom.Size) error {
	f.Lock()
	f.size = size
	f.Unlock()
	f.regenerate()
	return nil
}

func (f *Framer) Set(frame Frame) {
	f.Lock()
	f.frame = frame
	f.Unlock()
	f.regenerate()
}

func NewFramer(
	ctx context.Context,
	frame Frame,
) *Framer {
	f := &Framer{
		UpdatePublisher: mux.NewPublisher(),
		frame:           frame,
	}

	return f
}
