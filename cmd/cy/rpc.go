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
	JSON   bool
}

type RPCExecResponse struct {
}

// RPC executes an RPC call on the server over the given Connection.
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

// callRPC executes an RPC call and returns the result.
func (s *Server) callRPC(
	conn Connection,
	request *P.RPCRequestMessage,
) (interface{}, error) {
	handle := new(codec.MsgpackHandle)

	switch request.Name {
	case "exec":
		var args RPCExecArgs
		if err := codec.NewDecoderBytes(
			request.Args,
			handle,
		).Decode(&args); err != nil {
			return nil, err
		}

		_, err := s.cy.ExecuteOnBehalf(
			conn.Ctx(),
			tree.NodeID(args.Node),
			args.Code,
			args.Source,
		)
		if err != nil {
			return nil, err
		}

		return nil, nil
	}

	return nil, fmt.Errorf("unknown RPC: %s", request.Name)
}

// HandleRPC handles an RPC request, calling the appropriate function and
// encoding the response.
func (s *Server) HandleRPC(conn Connection, request *P.RPCRequestMessage) {
	response, err := s.callRPC(conn, request)
	if err != nil {
		return
	}

	var responseBytes []byte
	if response != nil {
		enc := codec.NewEncoderBytes(
			&responseBytes,
			new(codec.MsgpackHandle),
		)
		err = enc.Encode(response)
	}

	msg := P.RPCResponseMessage{
		Errored:  err != nil,
		Response: responseBytes,
	}

	if err != nil {
		msg.Error = err.Error()
	}

	conn.Send(msg)
}
