package stream

import (
	"context"
	"io"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux/screen"

	"github.com/xo/terminfo"
)

// A Renderer produces the stream of bytes necessary to render a Screen to a
// destination terminal. Conceptually, it is the opposite of a Screen, which
// takes an IO and feeds it to a virtual terminal emulator. A Renderer takes a
// Screen and makes an IO. This is useful because an IO may produce many more
// writes than are actually necessary to change the screen; a Renderer can make
// optimizations to only update the parts of the screen that have changed.
type Renderer struct {
	target emu.Terminal
	screen screen.Screen
	r      *io.PipeReader
	w      *io.PipeWriter
	info   *terminfo.Terminfo
}

var _ Stream = (*Renderer)(nil)

func (r *Renderer) Resize(size Size) error {
	r.target.Resize(size.C, size.R)
	err := r.screen.Resize(size)
	return err
}

func (r *Renderer) Write(data []byte) (n int, err error) {
	return r.screen.Write(data)
}

func (r *Renderer) Read(p []byte) (n int, err error) {
	return r.r.Read(p)
}

func (r *Renderer) poll(ctx context.Context) error {
	subscriber := r.screen.Updates()
	defer subscriber.Done()

	changes := subscriber.Recv()

	for {
		_, err := r.w.Write(tty.Swap(
			r.info,
			tty.Capture(r.target),
			r.screen.State(),
		))

		if err != nil {
			return err
		}

		select {
		case <-ctx.Done():
			return nil
		case <-changes:
			continue
		}
	}
}

func NewRenderer(
	ctx context.Context,
	info *terminfo.Terminfo,
	target emu.Terminal,
	screen Screen,
) *Renderer {
	r, w := io.Pipe()

	renderer := &Renderer{
		target: target,
		screen: screen,
		r:      r,
		w:      w,
		info:   info,
	}

	go renderer.poll(ctx)

	return renderer
}
