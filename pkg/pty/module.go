package pty

import (
	"context"
	"io"
	"os"
	"os/exec"

	"github.com/creack/pty"
)

type Pty struct {
	ptmx *os.File
	done chan error
}

func Run(
	ctx context.Context,
	command string,
	args []string,
	directory string,
	rows, cols int,
) (*Pty, error) {
	started := make(chan error)
	shellDone := make(chan error)
	p := &Pty{
		done: shellDone,
	}

	go func() {
		shell := exec.CommandContext(ctx, command, args...)
		shell.Dir = directory

		fd, err := pty.StartWithSize(
			shell,
			&pty.Winsize{
				Rows: uint16(rows),
				Cols: uint16(cols),
			},
		)
		if err != nil {
			started <- err
		}

		started <- nil

		p.ptmx = fd

		defer fd.Close()
		shellDone <- shell.Wait()
	}()

	err := <-started
	if err != nil {
		return nil, err
	}

	return p, nil
}

func (c *Pty) Wait() error {
	return <-c.done
}

func (c *Pty) Read(p []byte) (n int, err error) {
	return c.ptmx.Read(p)
}

func (c *Pty) Write(p []byte) (n int, err error) {
	return c.ptmx.Write(p)
}

func (p *Pty) Resize(rows, cols int) error {
	return pty.Setsize(p.ptmx, &pty.Winsize{
		Rows: uint16(rows),
		Cols: uint16(cols),
	})
}

var _ io.ReadWriter = (*Pty)(nil)
