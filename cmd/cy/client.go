package main

import (
	"context"
	"io"
	"os"
	"strings"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	I "github.com/cfoust/cy/pkg/io"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/stream/cli"

	"github.com/muesli/termenv"
	"golang.org/x/term"
)

const (
	ENOENT       = "no such file or directory"
	ECONNREFUSED = "connection refused"
)

type ClientIO struct {
	conn Connection
	r    *io.PipeReader
}

func (c *ClientIO) Write(p []byte) (n int, err error) {
	err = c.conn.Send(P.InputMessage{
		Data: p,
	})

	return len(p), err
}

func (c *ClientIO) Read(p []byte) (n int, err error) {
	return c.r.Read(p)
}

func (c *ClientIO) Resize(size geom.Vec2) error {
	return c.conn.Send(P.SizeMessage{
		Rows:    size.R,
		Columns: size.C,
	})
}

func (c *ClientIO) Kill() {
	// unused
}

var _ mux.Stream = (*ClientIO)(nil)

// Do some sanity checks on a shell string.
func checkShell(shell string) bool {
	if len(shell) == 0 || shell[0] != '/' {
		return false
	}

	/**
	TODO(cfoust): 07/22/23
	if (areshell(shell))
		return (0);
	if (access(shell, X_OK) != 0)
		return (0);
	**/

	return true
}

func getShell() string {
	env := os.Getenv("SHELL")
	if checkShell(env) {
		return env
	}

	pwShell, err := getUserShell(os.Getuid())
	if err == nil && checkShell(pwShell) {
		return pwShell
	}

	return "/bin/bash"
}

/**
if ((s = getenv("VISUAL")) != NULL || (s = getenv("EDITOR")) != NULL) {
	options_set_string(global_options, "editor", 0, "%s", s);
}
**/

func getEnv() map[string]string {
	env := make(map[string]string)

	for _, kv := range os.Environ() {
		key, value, found := strings.Cut(kv, "=")
		if !found {
			continue
		}
		env[key] = value
	}

	return env
}

func buildHandshake(profile termenv.Profile) (*P.HandshakeMessage, error) {
	columns, rows, err := term.GetSize(int(os.Stdin.Fd()))
	if err != nil {
		return nil, err
	}

	return &P.HandshakeMessage{
		Env:   getEnv(),
		Shell: getShell(),
		Size: geom.Size{
			R: rows,
			C: columns,
		},
		Profile: profile,
	}, nil
}

func poll(conn Connection) error {
	output := termenv.NewOutput(os.Stdout)

	handshake, err := buildHandshake(output.Profile)
	if err != nil {
		return err
	}

	_ = conn.Send(*handshake)

	r, w := io.Pipe()
	writer := &ClientIO{
		conn: conn,
		r:    r,
	}

	go func() {
		events := conn.Receive()
		for {
			select {
			case <-conn.Ctx().Done():
				return
			case packet := <-events:
				if packet.Error != nil {
					// TODO(cfoust): 12/25/23
					return
				}

				switch msg := packet.Contents.(type) {
				case *P.OutputMessage:
					_, _ = w.Write(msg.Data)
				case *P.CloseMessage:
					_ = conn.Close()
					return
				}
			}
		}
	}()

	return cli.Attach(
		conn.Ctx(),
		writer,
		os.Stdin,
		os.Stdout,
	)
}

func connect(socketPath string, shouldStart bool) (Connection, error) {
	// mimics client_connect() in tmux's client.c
	var lockFd *os.File
	var lockPath string

	locked := false
	started := false
	for {
		conn, err := ws.Connect(
			context.Background(),
			P.Protocol,
			socketPath,
		)
		if err == nil || !shouldStart {
			return conn, err
		}

		message := err.Error()
		if !strings.Contains(message, ENOENT) && !strings.Contains(message, ECONNREFUSED) {
			return nil, err
		}

		if !locked {
			lockPath = getLockPath(socketPath)
			lockFd, err = I.Lock(lockPath)
			if err != nil {
				if err == I.ErrorLockFailed {
					continue
				}

				lockFd = nil

				return nil, err
			}

			/*
			 * FROM TMUX:
			 * Always retry at least once, even if we got the lock,
			 * because another client could have taken the lock,
			 * started the server and released the lock between our
			 * connect() and flock().
			 */
			locked = true
			continue
		}

		if err := os.Remove(socketPath); err != nil && !strings.Contains(err.Error(), ENOENT) {
			return nil, err
		}

		if lockFd != nil {
			_ = lockFd.Close()
			_ = os.Remove(lockPath)
		}

		if !started {
			// Now we can start the server
			err = startServer(socketPath)
			if err != nil {
				return nil, err
			}
			started = true
		}

		time.Sleep(50 * time.Millisecond)
	}
}
