package cy

import (
	"io"
	"os"

	"github.com/cfoust/cy/pkg/pty"
	"github.com/cfoust/cy/pkg/session"

	"golang.org/x/term"
)

type Cy struct {
	pty     *pty.Pty
	session *session.Session
	writes  chan []byte
}

func New() *Cy {
	return &Cy{
		session: session.New(),
		writes:  make(chan []byte),
	}
}

// read from pty:
// 1. record in session.
// 2. send to virtual.
// 3. if not alt, write to main.

func (c *Cy) readPty(p *pty.Pty) {
	buffer := make([]byte, 4096)

	for {
		//if ctx.Err() != nil {
		//return
		//}

		numBytes, err := p.Read(buffer)
		if err == io.EOF {
			return
		}
		if err != nil {
			// TODO(cfoust): 05/17/23
			return
		}
		if numBytes == 0 {
			continue
		}

		copied := make([]byte, numBytes)
		copy(copied, buffer[:numBytes])
		c.session.Output(copied)
		c.write(copied)
	}
}

// Initialize cy using the command to start a pseudo-tty. This function only
// returns once the underlying pty is done.
func (c *Cy) Run(command string) error {
	p, err := pty.Run(command)
	if err != nil {
		return err
	}

	c.pty = p

	go c.readPty(p)

	return p.Wait()
}

func (c *Cy) write(data []byte) {
	c.writes <- data
}

func (c *Cy) Read(p []byte) (n int, err error) {
	data := <-c.writes
	copy(p, data)
	return len(data), nil
}

func (c *Cy) Write(p []byte) (n int, err error) {
	// TODO(cfoust): 05/18/23 if alt, process input
	c.session.Input(p)
	return c.pty.Write(p)
}

func (c *Cy) Resize(pty *os.File) error {
	width, height, err := term.GetSize(int(os.Stdin.Fd()))
	if err != nil {
		return err
	}

	c.session.Resize(width, height)

	err = c.pty.Resize(pty)
	if err != nil {
		return err
	}
	return nil
}

var _ io.ReadWriter = (*Cy)(nil)
