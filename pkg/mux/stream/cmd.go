package stream

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path"
	"time"

	"github.com/cfoust/cy/pkg/util"
	"github.com/cfoust/cy/pkg/util/dir"

	"github.com/creack/pty"
	"github.com/sasha-s/go-deadlock"
	"github.com/xo/terminfo"
)

type CmdOptions struct {
	Directory string
	Command   string
	Args      []string
	Restart   bool
	Env       map[string]string
}

type CmdStatus int

const (
	CmdStatusStarting CmdStatus = iota
	CmdStatusHealthy
	CmdStatusFailed
	CmdStatusComplete
	CmdStatusKilled
)

type Cmd struct {
	util.Lifetime
	deadlock.RWMutex

	status        CmdStatus
	statusUpdates *util.Publisher[CmdStatus]

	options CmdOptions
	size    Size

	ptmx *os.File
	proc *os.Process
	done chan error

	exitError error
}

var _ Stream = (*Cmd)(nil)

func (c *Cmd) Kill() {
	c.Cancel()
}

func (c *Cmd) Resize(size Size) error {
	c.Lock()
	c.size = size
	c.Unlock()

	return pty.Setsize(c.ptmx, &pty.Winsize{
		Rows: uint16(size.R),
		Cols: uint16(size.C),
	})
}

// Options returns the original arguments used to start the command.
func (c *Cmd) Options() CmdOptions {
	return c.options
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
	done := make(chan error)
	options := c.options
	size := c.size

	go func() {
		cmd := exec.CommandContext(
			ctx,
			options.Command,
			options.Args...,
		)
		cmd.Dir = options.Directory
		cmd.Env = append(
			os.Environ(),
			// TODO(cfoust): 08/08/23 this is complicated
			fmt.Sprintf("TERM=%s", TERM),
			// Old ncurses applications don't fall back to the
			// default terminfo location if they can't find
			// terminfo for the current terminal, they just
			// break, so we need to inject the correct TERMINFO
			// for xterm-256color
			fmt.Sprintf("TERMINFO=%s", TERMINFO_LOCATION),
		)

		for key, value := range options.Env {
			cmd.Env = append(
				cmd.Env,
				// TODO(cfoust): 08/17/24 escaping?
				fmt.Sprintf("%s=%s", key, value),
			)
		}

		fd, err := pty.StartWithSize(
			cmd,
			&pty.Winsize{
				Rows: uint16(size.R),
				Cols: uint16(size.C),
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
		done <- cmd.Wait()
	}()

	err := <-started
	if err != nil {
		return nil, err
	}

	return done, nil
}

// Run the pane's command and only return when it's finished.
func (c *Cmd) run(ctx context.Context) error {
	started := make(chan error)

	var done chan error
	go func() {
		c.setStatus(CmdStatusStarting)

		ptyDone, err := c.runPty(ctx)
		if err != nil {
			started <- err
		}

		done = ptyDone

		started <- nil
		c.setStatus(CmdStatusHealthy)
	}()

	err := <-started
	if err != nil {
		return err
	}

	return <-done
}

func (c *Cmd) Read(p []byte) (n int, err error) {
	c.RLock()
	var (
		ptmx      = c.ptmx
		status    = c.status
		exitError = c.exitError
	)
	c.RUnlock()

	if exitError != nil {
		return 0, exitError
	}

	if status != CmdStatusStarting && status != CmdStatusHealthy {
		return 0, io.EOF
	}

	if ptmx == nil {
		return 0, nil
	}

	n, err = ptmx.Read(p)
	if err == io.EOF {
		c.Lock()
		c.ptmx = nil
		c.Unlock()
		return c.Read(p)
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

func (c *Cmd) runOnce(ctx context.Context) {
	errChan := make(chan error)

	go func() {
		select {
		case <-ctx.Done():
		case errChan <- c.run(ctx):
		}
	}()

	select {
	case <-ctx.Done():
		c.setStatus(CmdStatusKilled)
		return
	case err := <-errChan:
		if err == nil {
			c.setStatus(CmdStatusComplete)
			return
		}

		c.Lock()
		c.status = CmdStatusFailed
		c.exitError = err
		c.Unlock()
	}
}

// spin runs the command over and over (exiting a shell or an editor will just
// start it again.) If it fails more than SPIN_NUM_TRIES in under
// SPIN_THRESHOLD, it will not be restarted.
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
			c.setStatus(CmdStatusComplete)
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

func (c *Cmd) waitHealthy(ctx context.Context) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	changes := c.statusUpdates.Subscribe(ctx)

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
					errc <- fmt.Errorf(
						"starting cmd failed: %d",
						status,
					)
					return
				}
			}
		}
	}()
	return <-errc
}

func NewCmd(ctx context.Context, options CmdOptions, size Size) (*Cmd, error) {
	lifetime := util.NewLifetime(ctx)
	cmd := Cmd{
		Lifetime:      lifetime,
		status:        CmdStatusStarting,
		options:       options,
		size:          size,
		statusUpdates: util.NewPublisher[CmdStatus](),
	}

	if options.Restart {
		go cmd.spin(lifetime.Ctx())
	} else {
		go cmd.runOnce(lifetime.Ctx())
	}

	err := cmd.waitHealthy(lifetime.Ctx())
	if err != nil {
		return nil, err
	}

	return &cmd, nil
}

const TERM = "xterm-256color"

var TERMINFO_LOCATION = ""

func init() {
	info, err := terminfo.Load(TERM)
	if err != nil {
		return
	}

	TERMINFO_LOCATION = path.Clean(path.Join(info.File, "..", ".."))
}
