package main

import (
	"context"
	"fmt"
	"io"

	"github.com/cfoust/cy/pkg/clipboard"
	"github.com/cfoust/cy/pkg/cy"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"

	"github.com/rs/zerolog/log"
	"github.com/sevlyar/go-daemon"
)

func getLockPath(socketPath string) string {
	return socketPath + ".lock"
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

func (s *Server) handleCyClient(
	conn Connection,
	ws *Client,
	handshake *P.HandshakeMessage,
) error {
	cy, err := s.cy.NewClient(conn.Ctx(), *handshake)
	if err != nil {
		return err
	}

	events := conn.Receive()

	go func() { _, _ = io.Copy(ws, cy) }()

	for {
		select {
		case <-conn.Ctx().Done():
			return nil
		case <-cy.Ctx().Done():
			ws.close()
			return nil
		case packet := <-events:
			if packet.Error != nil {
				// TODO(cfoust): 06/08/23 handle gracefully
				continue
			}

			switch packet.Contents.Type() {
			case P.MessageTypeSize:
				msg := packet.Contents.(*P.SizeMessage)
				cy.Resize(geom.Vec2{
					R: msg.Rows,
					C: msg.Columns,
				})

			case P.MessageTypeInput:
				msg := packet.Contents.(*P.InputMessage)
				_, err := cy.Write(msg.Data)
				if err != nil {
					return err
				}
			}
		}
	}
}

func (s *Server) HandleWSClient(conn Connection) {
	events := conn.Receive()

	wsClient := &Client{conn: conn}

	for {
		select {
		case <-conn.Ctx().Done():
			return
		case msg := <-events:
			switch msg := msg.Contents.(type) {
			case *P.HandshakeMessage:
				if err := s.handleCyClient(
					conn,
					wsClient,
					msg,
				); err != nil {
					wsClient.closeError(err)
					return
				}
				return
			case *P.RPCRequestMessage:
				s.HandleRPC(conn, msg)
			}
		}
	}

}

func serve(path string) error {
	clipboard, err := clipboard.NewSystemClipboard()
	if err != nil {
		return err
	}

	cy, err := cy.Start(context.Background(), cy.Options{
		SocketPath: path,
		SocketName: CLI.Socket,
		Config:     cy.FindConfig(),
		DataDir:    cy.FindDataDir(),
		Shell:      getShell(),
		Clipboard:  clipboard,
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
