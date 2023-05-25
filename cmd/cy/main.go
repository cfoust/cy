package main

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"syscall"
	"time"

	"github.com/cfoust/cy/pkg/cy"
	//"github.com/cfoust/cy/pkg/session"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/sevlyar/go-daemon"
	"github.com/xo/terminfo"
	"golang.org/x/term"
)

const (
	CY_SOCKET_ENV      = "CY"
	CY_SOCKET_TEMPLATE = "/tmp/cy-%d"
)

// Much of the socket creation code is ported from tmux. (see tmux.c)

func makeLabel() (string, error) {
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

func startServer() error {
	return nil
}

func main() {
	var socketPath string

	if envPath, ok := os.LookupEnv(CY_SOCKET_ENV); ok {
		socketPath = envPath
	}

	if socketPath == "" {
		label, err := makeLabel()
		if err != nil {
			log.Panic().Err(err).Msg("failed to daemonize")
		}
		socketPath = label
	}

	cntxt := &daemon.Context{}

	d, err := cntxt.Reborn()
	if err != nil {
		log.Panic().Err(err).Msg("failed to daemonize")
	}
	if d != nil {
		return
	}
	defer cntxt.Release()

	consoleWriter := zerolog.ConsoleWriter{Out: os.Stdout, TimeFormat: time.RFC3339}
	log.Logger = log.Output(consoleWriter)

	ti, err := terminfo.LoadFromEnv()
	if err != nil {
		log.Panic().Err(err).Msg("could not read terminal info")
	}

	buf := new(bytes.Buffer)
	ti.Fprintf(buf, terminfo.ClearScreen)
	ti.Fprintf(buf, terminfo.CursorHome)
	os.Stdout.Write(buf.Bytes())

	c, err := cy.Run("bash")
	if err != nil {
		log.Panic().Err(err).Msg("could not run cy")
	}

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

				c.Resize(os.Stdin)

				currentWidth = width
				currentHeight = height
			}
		}
	}()
	ch <- syscall.SIGWINCH
	defer func() { signal.Stop(ch); close(ch) }()

	// Change to raw mode
	oldState, err := term.MakeRaw(int(os.Stdin.Fd()))
	if err != nil {
		log.Panic().Err(err).Msg("could not enter raw mode")
	}

	go func() { _, _ = io.Copy(c, os.Stdin) }()
	go func() { _, _ = io.Copy(os.Stdout, c) }()

	c.Wait()
	term.Restore(int(os.Stdin.Fd()), oldState)
}
