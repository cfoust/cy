package renderer

import (
	"context"
	"io"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/xo/terminfo"
)

// A Renderer produces the stream of bytes necessary to render a Screen to a
// destination terminal. Conceptually, it is the opposite of a Screen, which
// takes a Stream and feeds it to a virtual terminal emulator. A Renderer takes
// a Screen and makes a Stream. This is useful because a Stream may produce
// many more writes than are actually necessary to change the screen; a
// Renderer can make optimizations to only update the parts of the screen that
// have changed.
type Renderer struct {
	raw    emu.Terminal
	screen screen.Screen
	r      *io.PipeReader
	w      *io.PipeWriter
	info   *terminfo.Terminfo
}

var _ mux.Stream = (*Renderer)(nil)

func (r *Renderer) Kill() {
	r.screen.Kill()
}

func (r *Renderer) clearScreen(w io.Writer) {
	r.info.Fprintf(w, terminfo.ClearScreen)
	r.info.Fprintf(w, terminfo.CursorHome)
}

func (r *Renderer) Resize(size geom.Size) error {
	r.raw.Resize(size)
	r.clearScreen(r.raw)
	r.clearScreen(r.w)
	return r.screen.Resize(size)
}

func (r *Renderer) Send(msg mux.Msg) {
	r.screen.Send(msg)
}

func (r *Renderer) Write(data []byte) (n int, err error) {
	for i, w := 0, 0; i < len(data); i += w {
		var msg taro.Msg
		w, msg = taro.DetectOneMsg(data[i:])
		r.Send(msg)
	}

	return len(data), nil
}

func (r *Renderer) Read(p []byte) (n int, err error) {
	return r.r.Read(p)
}

func (r *Renderer) poll(ctx context.Context) error {
	subscriber := r.screen.Subscribe(ctx)

	for {
		changes := tty.Swap(
			r.info,
			tty.Capture(r.raw),
			r.screen.State(),
		)
		r.raw.Write(changes)
		_, err := r.w.Write(changes)
		if err != nil {
			return err
		}

		select {
		case <-ctx.Done():
			return nil
		case <-subscriber.Recv():
			continue
		}
	}
}

func NewRenderer(
	ctx context.Context,
	info *terminfo.Terminfo,
	initialSize geom.Size,
	screen mux.Screen,
) *Renderer {
	r, w := io.Pipe()
	target := emu.New(emu.WithSize(initialSize))
	screen.Resize(initialSize)
	renderer := &Renderer{
		raw:    target,
		screen: screen,
		r:      r,
		w:      w,
		info:   info,
	}

	go renderer.poll(ctx)

	return renderer
}
