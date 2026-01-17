package cy

import (
	"context"
	"io"
	"os"

	"github.com/cfoust/cy/pkg/janet"
)

// logPipe redirects Janet's :out and :err dynamic streams to an io.Writer
// (typically the logs pane). This allows print and eprint calls from user
// Janet code to appear in cy's logs.
type logPipe struct {
	vm *janet.VM

	stdoutR *os.File
	stdoutW *os.File
	stderrR *os.File
	stderrW *os.File

	outValue *janet.Value
	errValue *janet.Value

	dyns map[janet.Keyword]any
}

func newLogPipe(
	ctx context.Context,
	vm *janet.VM,
	dest io.Writer,
) (*logPipe, error) {
	stdoutR, stdoutW, err := os.Pipe()
	if err != nil {
		return nil, err
	}

	stderrR, stderrW, err := os.Pipe()
	if err != nil {
		_ = stdoutR.Close()
		_ = stdoutW.Close()
		return nil, err
	}

	lp := &logPipe{
		vm:      vm,
		stdoutR: stdoutR,
		stdoutW: stdoutW,
		stderrR: stderrR,
		stderrW: stderrW,
	}

	lp.outValue, err = vm.WrapFile(
		ctx,
		lp.stdoutW,
		janet.FileFlagWrite,
	)
	if err != nil {
		lp.Close()
		return nil, err
	}

	lp.errValue, err = vm.WrapFile(
		ctx,
		lp.stderrW,
		janet.FileFlagWrite,
	)
	if err != nil {
		lp.Close()
		return nil, err
	}

	lp.dyns = map[janet.Keyword]any{
		janet.Keyword("out"): lp.outValue,
		janet.Keyword("err"): lp.errValue,
	}

	go lp.stream(ctx, lp.stdoutR, dest)
	go lp.stream(ctx, lp.stderrR, dest)

	return lp, nil
}

func (lp *logPipe) stream(ctx context.Context, r *os.File, dest io.Writer) {
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
	return lp.dyns
}

func (lp *logPipe) Close() {
	if lp == nil {
		return
	}

	if lp.stdoutR != nil {
		_ = lp.stdoutR.Close()
	}
	if lp.stdoutW != nil {
		_ = lp.stdoutW.Close()
	}
	if lp.stderrR != nil {
		_ = lp.stderrR.Close()
	}
	if lp.stderrW != nil {
		_ = lp.stderrW.Close()
	}

	if lp.outValue != nil {
		lp.outValue.Free()
	}
	if lp.errValue != nil {
		lp.errValue.Free()
	}
}
