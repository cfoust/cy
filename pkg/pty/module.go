package pty

import (
	"io"
	"os"

	"github.com/creack/pty"
)

type Pty struct {
	pty *os.File
}

func New() *Pty {
	return &Pty{}
}

// Initialize cy using the command to start a pseudo-tty.
func (p *Pty) Run(command string) error {
	return nil
}

func (c *Pty) Read(p []byte) (n int, err error) {
	return c.pty.Read(p)
}

func (c *Pty) Write(p []byte) (n int, err error) {
	return c.pty.Write(p)
}

func (p *Pty) Resize(f *os.File) error {
	return pty.InheritSize(os.Stdin, p.pty)
}

var _ io.ReadWriter = (*Pty)(nil)
