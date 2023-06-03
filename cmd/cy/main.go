package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"syscall"

	"github.com/cfoust/cy/pkg/cy"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"

	"github.com/rs/zerolog/log"
	"github.com/sevlyar/go-daemon"
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

func startServer(path string) error {
	cntxt := &daemon.Context{}

	d, err := cntxt.Reborn()
	if err != nil {
		log.Panic().Err(err).Msg("failed to daemonize")
	}
	if d != nil {
		return nil
	}
	//defer cntxt.Release()

	cy := cy.Cy{}
	return ws.Serve(context.Background(), path, &cy)
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
	log.Info().Msgf("%+v", path)
	rawConn, err := ws.Connect(context.Background(), path)
	if err != nil {
		err = startServer(path)
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
		TERM: "xterm256-color",
	})

	writer := ClientIO{
		conn: conn,
	}

	go func() { _, _ = io.Copy(&writer, os.Stdin) }()

	for {
		select {
		case <-rawConn.Ctx().Done():
			log.Info().Msgf("connection done")
			return nil
		case packet := <-conn.Receive():
			if packet.Error != nil {
				return packet.Error
			}
			log.Info().Msgf("%+v", packet.Contents)

			if msg, ok := packet.Contents.(*P.OutputMessage); ok {
				os.Stdout.Write(msg.Data)
			}
		}
	}
}

func main() {
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

	err := connect(socketPath)
	if err != nil {
		log.Panic().Err(err).Msg("failed to start cy")
	}

	//consoleWriter := zerolog.ConsoleWriter{Out: os.Stdout, TimeFormat: time.RFC3339}
	//log.Logger = log.Output(consoleWriter)

	//ti, err := terminfo.LoadFromEnv()
	//if err != nil {
	//log.Panic().Err(err).Msg("could not read terminal info")
	//}

	//buf := new(bytes.Buffer)
	//ti.Fprintf(buf, terminfo.ClearScreen)
	//ti.Fprintf(buf, terminfo.CursorHome)
	//os.Stdout.Write(buf.Bytes())

	//ch := make(chan os.Signal, 1)
	//signal.Notify(ch, syscall.SIGWINCH)
	//go func() {
	//currentHeight := 0
	//currentWidth := 0

	//for {
	//select {
	//case <-context.Background().Done():
	//return
	//case <-ch:
	//width, height, err := term.GetSize(int(os.Stdin.Fd()))
	//if err != nil {
	//log.Error().Err(err).Msg("failed to get terminal dimensions")
	//return
	//}

	//if width == currentWidth && height == currentHeight {
	//continue
	//}

	//c.Resize(os.Stdin)

	//currentWidth = width
	//currentHeight = height
	//}
	//}
	//}()
	//ch <- syscall.SIGWINCH
	//defer func() { signal.Stop(ch); close(ch) }()

	//// Change to raw mode
	//oldState, err := term.MakeRaw(int(os.Stdin.Fd()))
	//if err != nil {
	//log.Panic().Err(err).Msg("could not enter raw mode")
	//}

	//go func() { _, _ = io.Copy(c, os.Stdin) }()
	//go func() { _, _ = io.Copy(os.Stdout, c) }()

	//c.Wait()
	//term.Restore(int(os.Stdin.Fd()), oldState)

	//log.Info().Msgf("%+v", socketPath)
}
