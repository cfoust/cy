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

// Initialize cy using the command to start a pseudo-tty. This function only
// returns once the underlying pty is done.
func (c *Cy) Run(command string) error {
	p := pty.New()
	c.pty = p
	return p.Run(command)
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
	return 0, nil
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
