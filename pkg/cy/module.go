package cy

import (
	"io"
	"os"
	//"time"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/pty"
	"github.com/cfoust/cy/pkg/session"
	"github.com/cfoust/cy/pkg/util"

	"github.com/xo/terminfo"
	"golang.org/x/term"
)

type Cy struct {
	ti      *terminfo.Terminfo
	pty     *pty.Pty
	session *session.Session
	done    chan error

	buffer *util.WaitBuffer

	showUI bool

	// The terminal the user sees.
	Raw emu.Terminal
	// The terminal for the pty.
	Shell emu.Terminal
	// The terminal for cy's UI.
	UI emu.Terminal
}

func (c *Cy) readPty() {
	buffer := make([]byte, 4096)

	for {
		numBytes, err := c.pty.Read(buffer)
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
		_, err = c.Shell.Write(copied)
		if err != nil {
			return
		}

		c.write(copied)
	}
}

// Initialize cy using the command to start a pseudo-tty. This function only
// returns once the underlying pty is done.
func Run(command string) (*Cy, error) {
	started := make(chan error)
	ptyDone := make(chan error)

	ti, err := terminfo.LoadFromEnv()
	if err != nil {
		return nil, err
	}

	c := &Cy{
		session: session.New(),
		buffer:  util.NewWaitBuffer(),
		done:    ptyDone,
		ti:      ti,

		Raw:   emu.New(),
		Shell: emu.New(),
		UI:    emu.New(),
	}

	go func() {
		p, err := pty.Run(command)
		if err != nil {
			started <- err
		}

		c.pty = p

		started <- nil
	}()

	err = <-started
	if err != nil {
		return nil, err
	}

	go c.readPty()

	go func() {
		c.done <- c.pty.Wait()
	}()

	return c, nil
}

func (c *Cy) write(data []byte) {
	_, _ = c.Raw.Write(data)
	c.buffer.Write(data)
}

func (c *Cy) Read(p []byte) (int, error) {
	return c.buffer.Read(p)
}

func (c *Cy) Write(p []byte) (n int, err error) {
	for _, b := range p {
		if b == 6 {
			c.showUI = !c.showUI

			if !c.showUI {
				c.write(anim.SwapView(c.ti, c.Shell, c.Raw))
				return len(p), nil
			}

			c.write(anim.SwapView(c.ti, c.UI, c.Raw))
			c.write(anim.SwapView(c.ti, c.Shell, c.Raw))

			//go func() {
				//fade := anim.Fade(anim.CaptureImage(c.Raw))
				//steps := 30
				//for i := 0; i < steps; i++ {
					//c.write(
						//anim.Swap(
							//c.ti,
							//fade.Update(float32(i)/float32(steps)),
							//anim.CaptureImage(c.Raw),
						//),
					//)
					//time.Sleep(5 * time.Millisecond)
				//}
			//}()

			return len(p), nil
		}
	}

	if c.showUI {
		return len(p), nil
	}

	c.session.Input(p)
	return c.pty.Write(p)
}

func (c *Cy) Wait() error {
	return <-c.done
}

func (c *Cy) Session() *session.Session {
	return c.session
}

func (c *Cy) Resize(pty *os.File) error {
	width, height, err := term.GetSize(int(os.Stdin.Fd()))
	if err != nil {
		return err
	}

	c.session.Resize(width, height)
	c.Raw.Resize(width, height)
	c.Shell.Resize(width, height)
	c.UI.Resize(width, height)

	err = c.pty.Resize(pty)
	if err != nil {
		return err
	}
	return nil
}

var _ io.ReadWriter = (*Cy)(nil)
