package main

import (
	"os"

	"github.com/rs/zerolog/log"
	"github.com/sevlyar/go-daemon"
)

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
