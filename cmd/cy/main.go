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

type EventType byte

const (
	EventTypeInput EventType = iota
	EventTypeOutput
	EventTypeResize
)

type EventData interface {
	Type() EventType
}

type Write struct {
	Bytes []byte
}

type InputEvent Write

func (i InputEvent) Type() EventType { return EventTypeInput }

type OutputEvent Write

func (i OutputEvent) Type() EventType { return EventTypeOutput }

type ResizeEvent struct {
	Width  int
	Height int
}

func (i ResizeEvent) Type() EventType { return EventTypeResize }

type Event struct {
	Stamp time.Time
	Data  EventData
}

func proxy(ctx context.Context, dst io.Writer, src io.Reader) (<-chan []byte, error) {
	writes := make(chan []byte)

	go func() {
		buffer := make([]byte, 4096)
		for {
			if ctx.Err() != nil {
				return
			}

			numBytes, err := src.Read(buffer)
			if err == io.EOF {
				return
			}
			if err != nil {
				// TODO(cfoust): 05/17/23
				log.Error().Err(err).Msg("could not read from stream")
				return
			}
			if numBytes == 0 {
				continue
			}

			copied := make([]byte, numBytes)
			copy(copied, buffer[:numBytes])
			writes <- copied

			dst.Write(buffer[:numBytes])
		}
	}()

	return writes, nil
}

func proxyShell(ctx context.Context, command string, events chan Event) error {
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
				width, height, err := term.GetSize(int(os.Stdin.Fd()))
				if err != nil {
					log.Error().Err(err).Msg("failed to get terminal dimensions")
					return
				}
				events <- Event{
					Stamp: time.Now(),
					Data: ResizeEvent{
						Width:  width,
						Height: height,
					},
				}
				pty.InheritSize(os.Stdin, ptmx)
			}
		}
	}()
	ch <- syscall.SIGWINCH
	defer func() { signal.Stop(ch); close(ch) }()

	// Change to raw mode
	oldState, err := term.MakeRaw(int(os.Stdin.Fd()))
	if err != nil {
		return err
	}
	defer func() { _ = term.Restore(int(os.Stdin.Fd()), oldState) }()

	go func() {
		writes, err := proxy(ctx, ptmx, os.Stdin)
		if err != nil {
			log.Error().Err(err).Msg("failed to read user input")
			return
		}

		for {
			select {
			case <-ctx.Done():
				return
			case write := <-writes:
				events <- Event{
					Stamp: time.Now(),
					Data: InputEvent{
						Bytes: write,
					},
				}
			}
		}
	}()

	go func() {
		reads, err := proxy(ctx, os.Stdout, ptmx)
		if err != nil {
			log.Error().Err(err).Msg("failed to read shell output")
			return
		}

		for {
			select {
			case <-ctx.Done():
				return
			case read := <-reads:
				events <- Event{
					Stamp: time.Now(),
					Data: OutputEvent{
						Bytes: read,
					},
				}
			}
		}
	}()

	return shell.Wait()
}

func record(ctx context.Context, command string) (<-chan Event, <-chan error) {
	events := make(chan Event)
	done := make(chan error)

	go func() {
		done <- proxyShell(ctx, command, events)
	}()

	return events, done
}

func main() {
	consoleWriter := zerolog.ConsoleWriter{Out: os.Stdout, TimeFormat: time.RFC3339}
	log.Logger = log.Output(consoleWriter)

	events, done := record(context.Background(), "/bin/bash")

	eventList := make([]Event, 0)
	go func() {
		for {
			select {
			case <-context.Background().Done():
				return
			case event := <-events:
				eventList = append(eventList, event)
			}
		}
	}()

	err := <-done
	if err != nil {
		log.Panic().Err(err).Msg("shell exited with error")
	}

	for _, event := range eventList {
		log.Info().Msgf("%+v", event)
	}

	log.Info().Msgf("done")
}
