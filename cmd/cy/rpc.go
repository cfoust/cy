package main

import (
	"fmt"

	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/mux/screen/tree"

	"github.com/ugorji/go/codec"
)

type RPCExecArgs struct {
	Source string
	Node   int
	Code   []byte
	Dir    string
}

type RPCExecResponse struct {
}

// RPC executes an RPC call on the server.
func RPC[S any, T any](
	conn Connection,
	name string,
	args S,
) (T, error) {
	var result T

	msgs := conn.Subscribe(conn.Ctx())
	errc := make(chan error)
	response := make(chan *P.RPCResponseMessage)

	go func() {
		for {
			select {
			case <-conn.Ctx().Done():
				return
			case msg := <-msgs.Recv():
				if msg.Error != nil {
					errc <- msg.Error
					return
				}

				if msg.Contents.Type() != P.MessageTypeRPCResponse {
					continue
				}

				response <- msg.Contents.(*P.RPCResponseMessage)
			}
		}
	}()

	var payload []byte
	enc := codec.NewEncoderBytes(
		&payload,
		new(codec.MsgpackHandle),
	)
	if err := enc.Encode(args); err != nil {
		return result, err
	}

	conn.Send(P.RPCRequestMessage{
		Name: "exec",
		Args: payload,
	})

	select {
	case <-conn.Ctx().Done():
		return result, conn.Ctx().Err()
	case err := <-errc:
		return result, err
	case msg := <-response:
		if msg.Errored {
			return result, fmt.Errorf(msg.Error)
		}

		dec := codec.NewDecoderBytes(
			msg.Response,
			new(codec.MsgpackHandle),
		)
		if err := dec.Decode(&result); err != nil {
			return result, err
		}
	}

	return result, nil
}

func (s *Server) HandleRPC(conn Connection, msg *P.RPCRequestMessage) {
	handle := new(codec.MsgpackHandle)

	var responseBytes []byte
	var err error

	switch msg.Name {
	case "exec":
		var args RPCExecArgs
		if err = codec.NewDecoderBytes(
			msg.Args,
			handle,
		).Decode(&args); err != nil {
			break
		}

		_, err = s.cy.ExecuteOnBehalf(
			conn.Ctx(),
			tree.NodeID(args.Node),
			args.Code,
			args.Source,
		)
		if err != nil {
			break
		}

		enc := codec.NewEncoderBytes(&responseBytes, handle)
		if err = enc.Encode(nil); err != nil {
			return
		}
	default:
		err = fmt.Errorf("unknown RPC: %s", msg.Name)
	}

	response := P.RPCResponseMessage{
		Errored:  err != nil,
		Response: responseBytes,
	}

	if err != nil {
		response.Error = err.Error()
	}

	conn.Send(response)
}
