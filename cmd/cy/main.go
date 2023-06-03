package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"syscall"

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
	return ws.Serve(context.Background(), path, &cy)
}

func startServer(path string) (*os.Process, error) {
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
		return d, nil
	}
	defer cntxt.Release()

	return nil, err
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

func connect(path string) error {
	rawConn, err := ws.Connect(context.Background(), path)
	if err != nil {
		_, err := startServer(path)
		log.Info().Msgf("startServer returned")
		if err != nil {
			return err
		}

		return connect(path)
	}

	log.Info().Msgf("connected to cy")
	conn := ws.MapClient[[]byte](
		rawConn,
		P.Encode,
		P.Decode,
	)

	conn.Send(P.HandshakeMessage{
		TERM: os.Getenv("TERM"),
	})

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
		case <-rawConn.Ctx().Done():
			log.Info().Msgf("connection done")
			return nil
		case packet := <-conn.Receive():
			log.Info().Msgf("%+v", packet.Error)
			if packet.Error != nil {
				return packet.Error
			}

			if msg, ok := packet.Contents.(*P.OutputMessage); ok {
				os.Stdout.Write(msg.Data)
			}
		}
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

	err := connect(socketPath)
	if err != nil {
		log.Panic().Err(err).Msg("failed to start cy")
	}
}
