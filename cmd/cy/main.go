package main

import (
	"bytes"
	"context"
	"io"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/cfoust/cy/pkg/cy"
	"github.com/cfoust/cy/pkg/session"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/xo/terminfo"
	"golang.org/x/term"
)

func main() {
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
	defer func() { _ = term.Restore(int(os.Stdin.Fd()), oldState) }()

	go func() { _, _ = io.Copy(c, os.Stdin) }()
	go func() { _, _ = io.Copy(os.Stdout, c) }()

	err = c.Wait()
	if err != nil {
		log.Panic().Err(err).Msg("cy failed to exit")
	}

	for _, event := range c.Session().Events() {
		if data, ok := event.Data.(session.OutputEvent); ok {
			os.Stdout.Write(data.Bytes)
			time.Sleep(50 * time.Millisecond)
		}
	}
}
