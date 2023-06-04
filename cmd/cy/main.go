package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"github.com/cfoust/cy/pkg/cy"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"

	"github.com/rs/zerolog/log"
	"github.com/sevlyar/go-daemon"
	"golang.org/x/term"
)

const (
	CY_SOCKET_ENV      = "CY"
	CY_SOCKET_TEMPLATE = "/tmp/cy-%d"
)

// Much of the socket creation code is ported from tmux. (see tmux.c)
// Part laziness, part I wanted cy to be as familiar as possible.

func getSocketPath() (string, error) {
	uid := os.Getuid()
	directory := fmt.Sprintf(CY_SOCKET_TEMPLATE, uid)

	if err := os.MkdirAll(directory, syscall.S_IRWXU); err != nil {
		return "", err
	}

	info, err := os.Lstat(directory)
	if err != nil {
		return "", err
	}

	if !info.IsDir() {
		return "", fmt.Errorf("%s is not a directory", directory)
	}

	var stat syscall.Stat_t
	err = syscall.Stat(directory, &stat)
	if err != nil {
		return "", err
	}

	if stat.Uid != uint32(uid) || ((stat.Mode & syscall.S_IRWXO) != 0) {
		return "", fmt.Errorf("%s has unsafe permissions", directory)
	}

	label, err := filepath.Abs(filepath.Join(directory, "default"))
	if err != nil {
		return "", err
	}

	return label, nil
}

func serve(path string) error {
	log.Info().Msgf("serving cy")
	cy := cy.Cy{}
	return ws.Serve[P.Message](context.Background(), path, P.Protocol, &cy)
}

func startServer(path string) error {
	cntxt := &daemon.Context{
		LogFileName: "cy.log",
	}

	d, err := cntxt.Reborn()
	if daemon.WasReborn() {
		log.Info().Msgf("reborn")
	}
	if err != nil {
		log.Panic().Err(err).Msg("failed to daemonize")
	}
	if d != nil {
		return nil
	}
	defer cntxt.Release()

	return err
}

type ClientIO struct {
	conn cy.Connection
}

func (c *ClientIO) Write(p []byte) (n int, err error) {
	err = c.conn.Send(P.InputMessage{
		Data: p,
	})

	return len(p), err
}

var _ io.Writer = (*ClientIO)(nil)

func buildHandshake() (*P.HandshakeMessage, error) {
	columns, rows, err := term.GetSize(int(os.Stdin.Fd()))
	if err != nil {
		return nil, err
	}

	return &P.HandshakeMessage{
		TERM:    os.Getenv("TERM"),
		Rows:    rows,
		Columns: columns,
	}, nil
}

func poll(conn cy.Connection) error {
	log.Info().Msgf("connected to cy")

	handshake, err := buildHandshake()
	if err != nil {
		return err
	}

	conn.Send(*handshake)

	writer := ClientIO{
		conn: conn,
	}

	oldState, err := term.MakeRaw(int(os.Stdin.Fd()))
	if err != nil {
		return err
	}
	defer term.Restore(int(os.Stdin.Fd()), oldState)

	go func() { _, _ = io.Copy(&writer, os.Stdin) }()

	// Hanadle window size changes
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, syscall.SIGWINCH)
	go func() {
		currentHeight := 0
		currentWidth := 0

		for {
			select {
			case <-context.Background().Done():
				return
			case <-ch:
				width, height, err := term.GetSize(int(os.Stdin.Fd()))
				if err != nil {
					log.Error().Err(err).Msg("failed to get terminal dimensions")
					return
				}

				if width == currentWidth && height == currentHeight {
					continue
				}

				conn.Send(P.SizeMessage{
					Rows:    height,
					Columns: width,
				})

				currentWidth = width
				currentHeight = height
			}
		}
	}()
	ch <- syscall.SIGWINCH
	defer func() { signal.Stop(ch); close(ch) }()

	for {
		select {
		case <-conn.Ctx().Done():
			return nil
		case packet := <-conn.Receive():
			if packet.Error != nil {
				return packet.Error
			}

			if msg, ok := packet.Contents.(*P.OutputMessage); ok {
				os.Stdout.Write(msg.Data)
			}
		}
	}
}

const (
	ENOENT       = "no such file or directory"
	ECONNREFUSED = "connection refused"
)

var ErrorLockFailed = fmt.Errorf("failed to lock file")

func getLock(socketPath string) (*os.File, error) {
	lockPath := socketPath + ".lock"

	fd, err := os.OpenFile(lockPath, os.O_WRONLY|os.O_CREATE, 0600)
	if err != nil {
		return nil, err
	}

	if err := syscall.Flock(int(fd.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		fd.Close()
		return nil, ErrorLockFailed
	}

	return fd, nil
}

func connect(socketPath string) (cy.Connection, error) {
	// mimics client_connect() in tmux's client.c
	var lock *os.File = nil

	defer func() {
		if lock != nil {
			lock.Close()
		}
	}()

	locked := false
	for {
		conn, err := ws.Connect(context.Background(), P.Protocol, socketPath)
		if err == nil {
			return conn, nil
		}

		message := err.Error()
		if !strings.Contains(message, ENOENT) && !strings.Contains(message, ECONNREFUSED) {
			return nil, err
		}

		if !locked {
			lock, err = getLock(socketPath)
			if err != nil {
				if err == ErrorLockFailed {
					continue
				}

				lock = nil

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

		// Now we can start the server
		err = startServer(socketPath)
		if err != nil {
			return nil, err
		}

		time.Sleep(50 * time.Millisecond)
	}
}

func main() {
	log.Logger = log.Logger.With().Int("pid", os.Getpid()).Logger()

	var socketPath string

	if envPath, ok := os.LookupEnv(CY_SOCKET_ENV); ok {
		socketPath = envPath
	} else {
		label, err := getSocketPath()
		if err != nil {
			log.Panic().Err(err).Msg("failed to detect socket path")
		}
		socketPath = label
	}

	if daemon.WasReborn() {
		err := serve(socketPath)
		if err != nil {
			log.Panic().Err(err).Msg("failed to start cy")
		}
		return
	}

	conn, err := connect(socketPath)
	if err != nil {
		log.Panic().Err(err).Msg("failed to start cy")
	}

	err = poll(conn)
	if err != nil {
		log.Panic().Err(err).Msg("failed while polling")
	}
}
