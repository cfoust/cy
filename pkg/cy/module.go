package cy

import (
	"bytes"
	"io"
	"os"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/pty"
	"github.com/cfoust/cy/pkg/session"

	"github.com/xo/terminfo"
	"golang.org/x/term"
)

type Cy struct {
	ti      *terminfo.Terminfo
	pty     *pty.Pty
	session *session.Session
	writes  chan []byte
	done    chan error

	showUI bool

	// The terminal the user sees.
	Raw emu.Terminal
	// The terminal for the pty.
	Shell emu.Terminal
	// The terminal for cy's UI.
	UI emu.Terminal
}

// read from pty:
// 1. record in session.
// 2. send to virtual.
// 3. if not alt, write to main.

func (c *Cy) readPty() {
	buffer := make([]byte, 4096)

	for {
		//if ctx.Err() != nil {
		//return
		//}

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
		writes:  make(chan []byte),
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
	c.writes <- data
}

func (c *Cy) Read(p []byte) (n int, err error) {
	data := <-c.writes
	copy(p, data)

	// Mirror all writes to the conceptual shell
	_, err = c.Raw.Write(data)
	if err != nil {
		return 0, err
	}

	return len(data), nil
}

// Calculate the minimum string to transform `src` in to `dst`.
func Swap(
	info *terminfo.Terminfo,
	dst, src emu.View,
) []byte {
	width, height := src.Size()
	data := new(bytes.Buffer)

	info.Fprintf(data, terminfo.ClearScreen)
	info.Fprintf(data, terminfo.CursorHome)
	info.Fprintf(data, terminfo.CursorInvisible)

	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			dstCell := dst.Cell(x, y)
			srcCell := src.Cell(x, y)

			hasChar := dstCell.Char != srcCell.Char
			hasMode := dstCell.Mode != srcCell.Mode
			hasFG := dstCell.FG != srcCell.FG
			hasBG := dstCell.BG != srcCell.BG
			isDifferent := hasChar || hasMode || hasFG || hasBG

			if !isDifferent {
				continue
			}

			info.Fprintf(data, terminfo.CursorAddress, y, x)

			if hasFG || hasBG {
				info.Fprintf(data, terminfo.SetAForeground, int(dstCell.FG))
				info.Fprintf(data, terminfo.SetABackground, int(dstCell.BG))
			}

			data.Write([]byte(string(dstCell.Char)))

			info.Fprintf(data, terminfo.ExitAttributeMode)
		}
	}

	info.Fprintf(data, terminfo.CursorNormal)

	//srcCursor := src.Cursor()
	dstCursor := dst.Cursor()

	info.Fprintf(data, terminfo.CursorAddress, dstCursor.Y, dstCursor.X)

	return data.Bytes()
}

func (c *Cy) Write(p []byte) (n int, err error) {
	// TODO(cfoust): 05/18/23 if alt, process input
	for _, b := range p {
		if b == 6 {
			c.showUI = !c.showUI

			src := c.Raw
			dst := c.UI
			if !c.showUI {
				dst = c.Shell
			}

			c.write(Swap(c.ti, dst, src))
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
