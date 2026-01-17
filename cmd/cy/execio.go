package main

import (
	"context"
	"fmt"
	"io"

	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/janet"
)

// execIO wires Janet's dynamic streams to the websocket connection so that
// stdin/out/err are forwarded to the client.
type execIO struct {
	pipe *janet.Pipe
	done chan struct{}
}

func newExecIO(conn Connection, vm *janet.VM) (*execIO, error) {
	pipe, err := vm.Pipe(conn.Ctx())
	if err != nil {
		return nil, fmt.Errorf("failed to create pipe: %w", err)
	}

	e := &execIO{
		pipe: pipe,
		done: make(chan struct{}),
	}

	go e.streamOutput(conn.Ctx(), conn, pipe.Out, false)
	go e.streamOutput(conn.Ctx(), conn, pipe.Err, true)

	return e, nil
}

func (e *execIO) streamOutput(ctx context.Context, conn Connection, r io.Reader, stderr bool) {
	buf := make([]byte, 4096)
	for {
		select {
		case <-ctx.Done():
			return
		default:
		}

		n, err := r.Read(buf)
		if n > 0 {
			_ = conn.Send(P.OutputMessage{
				Data:   buf[:n],
				Stderr: stderr,
			})
		}

		if err != nil {
			return
		}
	}
}

func (e *execIO) Write(data []byte) error {
	if e == nil || e.pipe == nil || e.pipe.In == nil {
		return fmt.Errorf("stdin closed")
	}

	_, err := e.pipe.In.Write(data)
	return err
}

func (e *execIO) Dyns() map[janet.Keyword]any {
	return e.pipe.Dyns()
}

func (e *execIO) Done() <-chan struct{} {
	return e.done
}

func (e *execIO) Finish() {
	select {
	case <-e.done:
		return
	default:
		close(e.done)
	}
}

func (e *execIO) Close() {
	if e == nil {
		return
	}

	e.pipe.Close()
}
