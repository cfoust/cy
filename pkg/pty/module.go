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
) (*Pty, error) {
	started := make(chan error)
	shellDone := make(chan error)
	p := &Pty{
		done: shellDone,
	}

	go func() {
		shell := exec.CommandContext(ctx, command, args...)
		shell.Dir = directory

		fd, err := pty.Start(shell)
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

func (p *Pty) Resize(f *os.File) error {
	return pty.InheritSize(f, p.ptmx)
}

var _ io.ReadWriter = (*Pty)(nil)
