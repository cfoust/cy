package main

import (
	"context"
	"io"
	"os"
	"os/exec"
	"os/signal"
	"syscall"
	"time"

	"github.com/creack/pty"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"golang.org/x/term"
)

type Write struct {
	Data  []byte
	Stamp time.Time
}

func proxy(ctx context.Context, dst io.Writer, src io.Reader) (<-chan Write, error) {
	writes := make(chan Write)

	go func() {
		buffer := make([]byte, 4096)
		for {
			if ctx.Err() != nil {
				return
			}

			numBytes, err := src.Read(buffer)
			if numBytes == 0 {
				continue
			}

			log.Info().Msgf("%+v", string(buffer[:numBytes]))
			if err != nil {
				return
			}
			dst.Write(buffer[:numBytes])
		}
	}()

	return writes, nil
}

func record(ctx context.Context, command string) error {
	shell := exec.Command(command)
	ptmx, err := pty.Start(shell)
	if err != nil {
		return err
	}

	defer ptmx.Close()

	ch := make(chan os.Signal, 1)
	signal.Notify(ch, syscall.SIGWINCH)
	go func() {
		for {
			select {
			case <-ctx.Done():
				return
			case <-ch:
				pty.InheritSize(os.Stdin, ptmx)
			}
		}
	}()
	ch <- syscall.SIGWINCH
	defer func() { signal.Stop(ch); close(ch) }()

	// Change to raw mode
	oldState, err := term.MakeRaw(int(os.Stdin.Fd()))
	if err != nil {
		panic(err)
	}
	defer func() { _ = term.Restore(int(os.Stdin.Fd()), oldState) }() // Best effort.

	proxy(ctx, ptmx, os.Stdin)
	proxy(ctx, os.Stdout, ptmx)

	err = shell.Wait()
	if err != nil {
		return err
	}

	return nil
}

func main() {
	consoleWriter := zerolog.ConsoleWriter{Out: os.Stdout, TimeFormat: time.RFC3339}
	log.Logger = log.Output(consoleWriter)

	err := record(context.Background(), "/bin/bash")
	if err != nil {
		panic(err)
	}
}
