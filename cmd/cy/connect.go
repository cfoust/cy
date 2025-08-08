package main

import (
	"fmt"
	"net/http"
	"os"
	"runtime/pprof"
	"runtime/trace"

	"github.com/rs/zerolog/log"
	"github.com/sevlyar/go-daemon"
)

// connectCommand is the entrypoint for the connect command.
func connectCommand() error {
	if _, _, haveContext := getContext(); haveContext {
		return fmt.Errorf(
			"cy should be nested with care, unset $CY to force",
		)
	}

	var socketPath string

	label, err := getSocketPath(CLI.Socket)
	if err != nil {
		return fmt.Errorf(
			"failed to detect socket path: %s",
			err,
		)
	}
	socketPath = label

	if daemon.WasReborn() {
		cntx := new(daemon.Context)
		_, err := cntx.Reborn()
		if err != nil {
			return fmt.Errorf("failed to reincarnate")
		}

		defer func() {
			if err := cntx.Release(); err != nil {
				log.Panic().Err(err).Msg("unable to release pid-file")
			}
		}()

		if len(CLI.Connect.CPU) > 0 {
			f, err := os.Create(CLI.Connect.CPU)
			if err != nil {
				return fmt.Errorf(
					"unable to create %s: %s",
					CLI.Connect.CPU,
					err,
				)
			}
			defer func() { _ = f.Close() }()
			if err := pprof.StartCPUProfile(f); err != nil {
				return fmt.Errorf(
					"could not start CPU profile: %s",
					err,
				)
			}
			defer pprof.StopCPUProfile()
		}

		if len(CLI.Connect.Trace) > 0 {
			f, err := os.Create(CLI.Connect.Trace)
			if err != nil {
				return fmt.Errorf(
					"unable to create %s: %s",
					CLI.Connect.Trace,
					err,
				)
			}
			defer func() { _ = f.Close() }()
			if err := trace.Start(f); err != nil {
				return fmt.Errorf(
					"could not start trace profile: %s",
					err,
				)
			}
			defer trace.Stop()
		}

		err = serve(socketPath)
		if err != nil && err != http.ErrServerClosed {
			return fmt.Errorf(
				"failed to start cy: %s",
				err,
			)
		}
		return nil
	}

	conn, err := connect(socketPath, true)
	if err != nil {
		return fmt.Errorf(
			"failed to start cy: %s",
			err,
		)
	}

	err = poll(conn)
	if err != nil {
		return fmt.Errorf(
			"failed while polling: %s",
			err,
		)
	}

	return nil
}
