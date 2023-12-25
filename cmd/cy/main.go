package main

import (
	"net/http"
	"os"
	"runtime/pprof"
	"runtime/trace"

	"github.com/alecthomas/kong"
	"github.com/rs/zerolog/log"
	"github.com/sevlyar/go-daemon"
)

var CLI struct {
	Socket string `help:"Specify the name of the socket." name:"socket-name" optional:"" short:"L" default:"default"`

	CPU   string `help:"Save a CPU performance report to the given path." name:"perf-file" optional:"" default:""`
	Trace string `help:"Save a trace report to the given path." name:"trace-file" optional:"" default:""`
}

func main() {
	kong.Parse(&CLI,
		kong.Name("cy"),
		kong.Description("the time traveling terminal multiplexer"),
		kong.UsageOnError(),
		kong.ConfigureHelp(kong.HelpOptions{
			Compact: true,
			Summary: true,
		}))

	var socketPath string

	if envPath, ok := os.LookupEnv(CY_SOCKET_ENV); ok {
		socketPath = envPath
	} else {
		label, err := getSocketPath(CLI.Socket)
		if err != nil {
			log.Panic().Err(err).Msg("failed to detect socket path")
		}
		socketPath = label
	}

	if daemon.WasReborn() {
		cntx := new(daemon.Context)
		_, err := cntx.Reborn()
		if err != nil {
			log.Panic().Err(err).Msg("failed to reincarnate")
		}

		defer func() {
			if err := cntx.Release(); err != nil {
				log.Panic().Err(err).Msg("unable to release pid-file")
			}
		}()

		if len(CLI.CPU) > 0 {
			f, err := os.Create(CLI.CPU)
			if err != nil {
				log.Panic().Err(err).Msgf("unable to create %s", CLI.CPU)
			}
			defer f.Close()
			if err := pprof.StartCPUProfile(f); err != nil {
				log.Panic().Err(err).Msgf("could not start CPU profile")
			}
			defer pprof.StopCPUProfile()
		}

		if len(CLI.Trace) > 0 {
			f, err := os.Create(CLI.Trace)
			if err != nil {
				log.Panic().Err(err).Msgf("unable to create %s", CLI.Trace)
			}
			defer f.Close()
			if err := trace.Start(f); err != nil {
				log.Panic().Err(err).Msgf("could not start trace profile")
			}
			defer trace.Stop()
		}

		err = serve(socketPath)
		if err != nil && err != http.ErrServerClosed {
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
