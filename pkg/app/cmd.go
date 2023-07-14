package app

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"time"

	"github.com/cfoust/cy/pkg/util"
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
	size    Size

	statusUpdates *util.Publisher[CmdStatus]

	ptmx *os.File
	proc *os.Process
	done chan error
}

var _ IO = (*Cmd)(nil)

func (c *Cmd) getSize() Size {
	c.RLock()
	defer c.RUnlock()
	return c.size
}

func (c *Cmd) Resize(size Size) error {
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
	c.statusUpdates.Publish(status)
}

func (c *Cmd) runPty(ctx context.Context) (chan error, error) {
	started := make(chan error)
	shellDone := make(chan error)
	options := c.options
	size := c.size

	go func() {
		cmd := exec.CommandContext(ctx, options.Command, options.Args...)
		cmd.Dir = options.Directory
		cmd.Env = append(
			os.Environ(),
			// XXX temporary; to get around the waits in termenv
			// https://github.com/muesli/termenv/blob/master/termenv_unix.go#L230
			"TERM=tmux-256color",
		)

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

		c.Lock()
		c.ptmx = fd
		c.proc = cmd.Process
		c.Unlock()

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
	c.RLock()
	ptmx := c.ptmx
	c.RUnlock()

	if ptmx == nil {
		return 0, nil
	}

	n, err = ptmx.Read(p)
	if err == io.EOF {
		c.Lock()
		c.ptmx = nil
		c.Unlock()
		return 0, nil
	}

	return n, err
}

func (c *Cmd) Write(data []byte) (n int, err error) {
	c.RLock()
	ptmx := c.ptmx
	c.RUnlock()

	if ptmx == nil {
		return 0, nil
	}

	return ptmx.Write(data)
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

func (c *Cmd) Subscribe() *util.Subscriber[CmdStatus] {
	return c.statusUpdates.Subscribe()
}

func (c *Cmd) waitHealthy(ctx context.Context) error {
	changes := c.Subscribe()
	defer changes.Done()

	errc := make(chan error)
	go func() {
		for {
			select {
			case <-ctx.Done():
				errc <- ctx.Err()
				return
			case status := <-changes.Recv():
				switch status {
				case CmdStatusHealthy:
					errc <- nil
					return
				case CmdStatusFailed:
					errc <- fmt.Errorf("starting cmd failed: %d", status)
					return
				}
			}
		}
	}()
	return <-errc
}

func NewCmd(ctx context.Context, options CmdOptions, size Size) (*Cmd, error) {
	cmd := Cmd{
		status:        CmdStatusStarting,
		options:       options,
		size:          size,
		statusUpdates: util.NewPublisher[CmdStatus](),
	}

	go cmd.spin(ctx)

	err := cmd.waitHealthy(ctx)
	if err != nil {
		return nil, err
	}

	return &cmd, nil
}
