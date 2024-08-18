package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"syscall"
	"time"

	"github.com/cfoust/cy/pkg/cy"
	"github.com/cfoust/cy/pkg/geom"
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

type Server struct {
	cy *cy.Cy
}

type Connection = ws.Client[P.Message]

var _ ws.Server[P.Message] = (*Server)(nil)

type Client struct {
	conn Connection
}

func (c *Client) closeError(reason error) error {
	err := c.conn.Send(P.ErrorMessage{
		Message: fmt.Sprintf("error: %s", reason),
	})
	if err != nil {
		return err
	}

	return c.conn.Close()
}

func (c *Client) close() error {
	err := c.conn.Send(P.CloseMessage{})
	if err != nil {
		return err
	}

	return c.conn.Close()
}

func (c *Client) Write(data []byte) (n int, err error) {
	return len(data), c.conn.Send(P.OutputMessage{
		Data: data,
	})
}

func (s *Server) HandleWSClient(conn ws.Client[P.Message]) {
	events := conn.Subscribe(conn.Ctx())

	// First we need to wait for the client's handshake to know how to
	// handle its terminal
	handshakeCtx, cancel := context.WithTimeout(conn.Ctx(), 1*time.Second)
	defer cancel()

	wsClient := &Client{conn: conn}
	var client *cy.Client
	var err error

	select {
	case <-conn.Ctx().Done():
		return
	case <-handshakeCtx.Done():
		wsClient.closeError(fmt.Errorf("no handshake received"))
		return
	case msg := <-events.Recv():
		switch msg := msg.Contents.(type) {
		case *P.HandshakeMessage:
			client, err = s.cy.NewClient(conn.Ctx(), *msg)
		case *P.RPCRequestMessage:
			s.HandleRPC(conn, msg)
			return
		default:
			err = fmt.Errorf("must send handshake first")
		}

		if err != nil {
			wsClient.closeError(err)
			return
		}
	}

	go func() { _, _ = io.Copy(wsClient, client) }()

	for {
		select {
		case <-conn.Ctx().Done():
			return
		case <-client.Ctx().Done():
			wsClient.close()
			return
		case packet := <-events.Recv():
			if packet.Error != nil {
				// TODO(cfoust): 06/08/23 handle gracefully
				continue
			}

			switch packet.Contents.Type() {
			case P.MessageTypeSize:
				msg := packet.Contents.(*P.SizeMessage)
				client.Resize(geom.Vec2{
					R: msg.Rows,
					C: msg.Columns,
				})

			case P.MessageTypeInput:
				msg := packet.Contents.(*P.InputMessage)
				_, err := client.Write(msg.Data)
				if err != nil {
					wsClient.closeError(err)
					return
				}
			}
		}
	}
}

func serve(path string) error {
	cy, err := cy.Start(context.Background(), cy.Options{
		SocketPath: path,
		SocketName: CLI.Socket,
		Config:     cy.FindConfig(),
		DataDir:    cy.FindDataDir(),
		Shell:      getShell(),
	})
	if err != nil {
		return err
	}

	server := &Server{cy: cy}

	return ws.Serve[P.Message](cy.Ctx(), path, P.Protocol, server)
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
