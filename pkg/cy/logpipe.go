package cy

import (
	"context"
	"io"

	"github.com/cfoust/cy/pkg/janet"
)

// logPipe redirects Janet's :out and :err dynamic streams to an io.Writer
// (typically the logs pane). This allows print and eprint calls from user
// Janet code to appear in cy's logs.
type logPipe struct {
	pipe *janet.Pipe
}

func newLogPipe(
	ctx context.Context,
	vm *janet.VM,
	dest io.Writer,
) (*logPipe, error) {
	pipe, err := vm.Pipe(ctx)
	if err != nil {
		return nil, err
	}

	lp := &logPipe{pipe: pipe}

	go lp.stream(ctx, pipe.Out, dest)
	go lp.stream(ctx, pipe.Err, dest)

	return lp, nil
}

func (lp *logPipe) stream(ctx context.Context, r io.Reader, dest io.Writer) {
	buf := make([]byte, 4096)
	for {
		select {
		case <-ctx.Done():
			return
		default:
		}

		n, err := r.Read(buf)
		if n > 0 {
			_, _ = dest.Write(buf[:n])
		}

		if err != nil {
			return
		}
	}
}

// Dyns returns a map of dynamic bindings for :out and :err that can be passed
// to Janet code execution.
func (lp *logPipe) Dyns() map[janet.Keyword]any {
	return lp.pipe.Dyns()
}

func (lp *logPipe) Close() {
	if lp == nil {
		return
	}

	lp.pipe.Close()
}
