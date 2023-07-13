package app

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/util/dir"

	"github.com/creack/pty"
	"github.com/sasha-s/go-deadlock"
)

type CmdOptions struct {
	Directory string
	Command   string
	Args      []string
}

type CmdStatus int

const (
	CmdStatusStarting CmdStatus = iota
	CmdStatusHealthy
	CmdStatusFailed
)

type Cmd struct {
	deadlock.RWMutex

	status  CmdStatus
	options CmdOptions
	size    geom.Size

	ptmx *os.File
	proc *os.Process
	done chan error
}

var _ App = (*Cmd)(nil)

func (c *Cmd) getSize() geom.Size {
	c.RLock()
	defer c.RUnlock()
	return c.size
}

func (c *Cmd) Resize(size geom.Size) error {
	c.Lock()
	c.size = size
	c.Unlock()

	return pty.Setsize(c.ptmx, &pty.Winsize{
		Rows: uint16(size.Rows),
		Cols: uint16(size.Columns),
	})
}

func (c *Cmd) GetStatus() CmdStatus {
	c.RLock()
	status := c.status
	c.RUnlock()
	return status
}

func (c *Cmd) Path() (string, error) {
	c.RLock()
	proc := c.proc
	c.RUnlock()
	if proc == nil {
		return "", fmt.Errorf("process not yet started")
	}

	return dir.ForPid(proc.Pid)
}

func (c *Cmd) setStatus(status CmdStatus) {
	c.Lock()
	c.status = status
	c.Unlock()
}

func (c *Cmd) runPty(ctx context.Context) (chan error, error) {
	started := make(chan error)
	shellDone := make(chan error)
	options := c.options
	size := c.size

	go func() {
		cmd := exec.CommandContext(ctx, options.Command, options.Args...)
		cmd.Dir = options.Directory

		fd, err := pty.StartWithSize(
			cmd,
			&pty.Winsize{
				Rows: uint16(size.Rows),
				Cols: uint16(size.Columns),
			},
		)
		if err != nil {
			started <- err
		}

		started <- nil

		c.ptmx = fd
		c.proc = cmd.Process

		defer fd.Close()
		shellDone <- cmd.Wait()
	}()

	err := <-started
	if err != nil {
		return nil, err
	}

	return shellDone, nil
}

// Run the pane's command and only return when it's finished.
func (c *Cmd) run(ctx context.Context) error {
	started := make(chan error)

	var shellDone chan error
	go func() {
		c.setStatus(CmdStatusStarting)

		done, err := c.runPty(ctx)
		if err != nil {
			started <- err
		}

		shellDone = done

		started <- nil
		c.setStatus(CmdStatusHealthy)
	}()

	err := <-started
	if err != nil {
		return err
	}

	return <-shellDone
}

func (c *Cmd) Read(p []byte) (n int, err error) {
	return c.ptmx.Read(p)
}

func (c *Cmd) Write(data []byte) (n int, err error) {
	return c.ptmx.Write(data)
}

const (
	SPIN_NUM_TRIES = 3
	SPIN_THRESHOLD = 1 * time.Second
)

// Run the pane's pty command over and over (exiting a shell or an editor will
// just start it again.) If it fails more than SPIN_NUM_TRIES in under
// SPIN_THRESHOLD, don't restart it.
func (c *Cmd) spin(ctx context.Context) {
	errChan := make(chan error)

	startTime := time.Now()
	numErrors := 0

	for {
		go func() {
			errChan <- c.run(ctx)
		}()

		select {
		case <-ctx.Done():
			return
		case err := <-errChan:
			if err == nil {
				continue
			}

			elapsed := time.Now().Sub(startTime)

			// Reset the clock if it's been a while since the last error
			// This is just to guard against commands that suddenly stop working
			if elapsed > SPIN_THRESHOLD {
				numErrors = 0
				startTime = time.Now()
			}

			numErrors += 1

			if elapsed < SPIN_THRESHOLD && numErrors == SPIN_NUM_TRIES {
				c.setStatus(CmdStatusFailed)

				// TODO(cfoust): 07/12/23 restore this
				//p.Terminal.Write([]byte(fmt.Sprintf(
				//"\ncommand '%s' failed, backing off",
				//p.options.Command,
				//)))
				return
			}
		}
	}
}

func NewCmd(ctx context.Context, options CmdOptions, size geom.Size) *Cmd {
	pane := Cmd{
		status:  CmdStatusStarting,
		options: options,
		size:    size,
	}

	go pane.spin(ctx)

	return &pane
}
