package main

import (
	"net/http"
	"os"

	"github.com/alecthomas/kong"
	"github.com/rs/zerolog/log"
	"github.com/sevlyar/go-daemon"
)

var CLI struct {
	Socket string `help:"Specify the name of the socket." name:"socket-name" optional:"" short:"L" default:"default"`
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

	log.Logger = log.Logger.With().Int("pid", os.Getpid()).Logger()

	if isStories() {
		startStories()
		return
	}

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
		err := serve(socketPath)
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
