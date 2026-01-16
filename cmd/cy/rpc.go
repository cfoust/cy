package main

import (
	"errors"
	"fmt"
	"time"

	"github.com/cfoust/cy/pkg/cy/api"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"

	"github.com/ugorji/go/codec"
)

type OutputFormat int

const (
	OutputFormatRaw OutputFormat = iota
	OutputFormatJSON
	OutputFormatJanet
)

const (
	RPCExec   = "exec"
	RPCOutput = "output"
)

type RPCExecArgs struct {
	Source string
	// The NodeID of a tree node, which will be used to infer which client
	// on behalf of whom the code will be run.
	Node   int
	Code   []byte
	Format OutputFormat
}

type RPCExecResponse struct {
	Data []byte
}

type RPCOutputArgs struct {
	Node  int
	Index int
}

type RPCOutputResponse struct {
	Data []byte
}

// RPC executes an RPC call on the server over the given Connection.
func RPC[S any, T any](
	conn Connection,
	name string,
	args S,
) (T, error) {
	var result T

	msgs := conn.Receive()
	errc := make(chan error)
	response := make(chan *P.RPCResponseMessage)

	go func() {
		for {
			select {
			case <-conn.Ctx().Done():
				return
			case msg := <-msgs:
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

	err := conn.Send(P.RPCRequestMessage{
		Name: name,
		Args: payload,
	})
	if err != nil {
		return result, err
	}

	done := make(chan struct{}, 1)
	go func() {
		<-conn.Ctx().Done()
		time.Sleep(1 * time.Second)
		done <- struct{}{}
	}()

	select {
	case <-done:
		return result, fmt.Errorf(
			"connection closed by server",
		)
	case err := <-errc:
		return result, err
	case msg := <-response:
		if msg.Errored {
			return result, errors.New(msg.Error)
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
	case RPCExec:
		var args RPCExecArgs
		if err := codec.NewDecoderBytes(
			request.Args,
			handle,
		).Decode(&args); err != nil {
			return nil, err
		}

		var context interface{} = nil
		sourceNodeID := tree.NodeID(args.Node)
		if client, found := s.cy.InferClient(sourceNodeID); found {
			if args.Node != 0 {
				context = &api.ExecContext{
					Client:     client,
					SourceNode: &sourceNodeID,
				}
			} else {
				context = client
			}
		}

		result, err := s.cy.ExecuteCall(
			conn.Ctx(),
			context,
			janet.Call{
				Code:       args.Code,
				SourcePath: args.Source,
			},
		)
		if err != nil {
			return nil, err
		}
		response := RPCExecResponse{}

		if result.Yield == nil {
			return response, nil
		}

		defer result.Yield.Free()

		switch args.Format {
		case OutputFormatRaw:
			response.Data, err = result.Yield.Raw()
			if err != nil {
				return nil, err
			}
			return response, nil
		case OutputFormatJanet:
			response.Data = []byte(result.Yield.String())
			return response, nil
		case OutputFormatJSON:
			response.Data, err = result.Yield.JSON()
			if err != nil {
				return nil, err
			}
			return response, nil
		default:
			return nil, fmt.Errorf(
				"unknown output format: %d",
				args.Format,
			)
		}
	case RPCOutput:
		var args RPCOutputArgs
		if err := codec.NewDecoderBytes(
			request.Args,
			handle,
		).Decode(&args); err != nil {
			return nil, err
		}

		data, err := s.cy.Output(
			tree.NodeID(args.Node),
			args.Index,
		)
		if err != nil {
			return nil, err
		}

		return RPCOutputResponse{
			Data: data,
		}, nil
	}

	return nil, fmt.Errorf("unknown RPC: %s", request.Name)
}

// HandleRPC handles an RPC request, calling the appropriate function and
// encoding the response.
func (s *Server) HandleRPC(conn Connection, request *P.RPCRequestMessage) {
	response, err := s.callRPC(conn, request)

	if err == nil && response == nil {
		err = fmt.Errorf(
			"no response from RPC call %s",
			request.Name,
		)
	}

	if err != nil {
		_ = conn.Send(P.RPCResponseMessage{
			Errored: err != nil,
			Error:   err.Error(),
		})
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

	_ = conn.Send(msg)
}
