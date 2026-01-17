package cli

import (
	"context"
	"io"
	"os"
	"os/signal"
	"syscall"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"

	"github.com/muesli/termenv"
	"github.com/xo/terminfo"
	"golang.org/x/term"
)

// Attach connects a mux.Stream to a tty defined by `in` and `out`, which are
// typically `os.Stdin` and `os.Stdout`.
func Attach(
	ctx context.Context,
	stream mux.Stream,
	in, out *os.File,
) error {
	output := termenv.NewOutput(out)

	info, err := terminfo.LoadFromEnv()
	if err != nil {
		panic(err)
	}

	output.AltScreen()
	output.EnableMouseAllMotion()
	output.EnableBracketedPaste()
	oldState, err := term.MakeRaw(int(in.Fd()))
	if err != nil {
		return err
	}
	defer func() {
		output.ExitAltScreen()
		output.DisableMouseAllMotion()
		output.DisableBracketedPaste()
		info.Fprintf(out, terminfo.CursorVisible)
		_ = term.Restore(int(in.Fd()), oldState)
	}()

	protocolDetector := &kittyDetector{
		r: in,
		w: out,
	}

	if err := protocolDetector.Query(); err != nil {
		return err
	}

	go func() { _, _ = io.Copy(stream, protocolDetector) }()
	go func() { _, _ = io.Copy(out, stream) }()

	// Handle window size changes
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, syscall.SIGWINCH)
	go func() {
		currentRows := 0
		currentCols := 0

		for {
			select {
			case <-ctx.Done():
				return
			case <-ch:
				columns, rows, err := term.GetSize(int(in.Fd()))
				if err != nil {
					// TODO(cfoust): 11/10/23 error handling
					return
				}

				if columns == currentCols && rows == currentRows {
					continue
				}

				// TODO(cfoust): 11/10/23 error handling
				_ = stream.Resize(geom.Size{
					R: rows,
					C: columns,
				})

				currentCols = columns
				currentRows = rows
			}
		}
	}()
	ch <- syscall.SIGWINCH
	defer func() { signal.Stop(ch); close(ch) }()

	<-ctx.Done()
	return nil
}
