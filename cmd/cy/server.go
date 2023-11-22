package main

import (
	"context"
	"fmt"
	"os"
	"syscall"

	"github.com/cfoust/cy/pkg/cy"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"

	"github.com/rs/zerolog/log"
	"github.com/sevlyar/go-daemon"
)

var ErrorLockFailed = fmt.Errorf("failed to lock file")

func getLockPath(socketPath string) string {
	return socketPath + ".lock"
}

func getLock(lockPath string) (*os.File, error) {
	fd, err := os.OpenFile(lockPath, os.O_WRONLY|os.O_CREATE, 0600)
	if err != nil {
		return nil, err
	}

	if err := syscall.Flock(int(fd.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		fd.Close()
		return nil, ErrorLockFailed
	}

	return fd, nil
}

func serve(path string) error {
	cy, err := cy.Start(context.Background(), cy.Options{
		Config:  findConfig(),
		DataDir: findDataDir(),
		Shell:   getShell(),
	})
	if err != nil {
		return err
	}

	return ws.Serve[P.Message](cy.Ctx(), path, P.Protocol, cy)
}

func startServer(path string) error {
	cntxt := &daemon.Context{
		LogFileName: fmt.Sprintf("%s.log", path),
		PidFileName: fmt.Sprintf("%s.pid", path),
	}

	_, err := cntxt.Reborn()
	if err != nil {
		log.Panic().Err(err).Msg("failed to daemonize")
	}

	return nil
}
