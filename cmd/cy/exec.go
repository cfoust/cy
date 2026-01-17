package main

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"sync"

	"github.com/cfoust/cy/pkg/cy"
	P "github.com/cfoust/cy/pkg/io/protocol"

	"github.com/ugorji/go/codec"
)

func getContext() (socket string, id int, ok bool) {
	context, ok := os.LookupEnv(cy.CONTEXT_ENV)
	if !ok {
		return "", 0, false
	}

	match := cy.CONTEXT_REGEX.FindStringSubmatch(context)
	if match == nil {
		return "", 0, false
	}

	socket = match[cy.CONTEXT_REGEX.SubexpIndex("socket")]
	id, _ = strconv.Atoi(match[cy.CONTEXT_REGEX.SubexpIndex("id")])
	ok = true
	return
}

// execCommand is the entrypoint for the exec command.
func execCommand() error {
	if CLI.Exec.Command == "" && CLI.Exec.File == "" {
		return fmt.Errorf("no Janet code provided")
	}

	var err error
	var source string
	var code []byte

	if CLI.Exec.Command != "" {
		source = "<unknown>"
		code = []byte(CLI.Exec.Command)
	} else if CLI.Exec.File == "-" {
		source = "<stdin>"
		code, err = io.ReadAll(os.Stdin)
		if err != nil {
			return fmt.Errorf("failed to read from stdin: %s", err)
		}
	} else {
		source = CLI.Exec.File
		code, err = os.ReadFile(CLI.Exec.File)
		if err != nil {
			return fmt.Errorf("failed to read from %s: %s", CLI.Exec.File, err)
		}
	}

	socket, id, ok := getContext()
	if !ok {
		socket = CLI.Socket
	}

	socketPath, err := getSocketPath(socket)
	if err != nil {
		return err
	}

	var conn Connection
	conn, err = connect(socketPath, false)
	if err != nil {
		return err
	}

	var format OutputFormat
	switch CLI.Exec.Format {
	case "raw":
		format = OutputFormatRaw
	case "json":
		format = OutputFormatJSON
	case "janet":
		format = OutputFormatJanet
	default:
		return fmt.Errorf(
			"unknown output format: %s",
			CLI.Exec.Format,
		)
	}

	payload := make([]byte, 0)
	enc := codec.NewEncoderBytes(
		&payload,
		new(codec.MsgpackHandle),
	)
	err = enc.Encode(RPCExecArgs{
		Source: source,
		Code:   code,
		Node:   id,
		Format: format,
	})
	if err != nil {
		return err
	}

	if err := conn.Send(P.RPCRequestMessage{
		Name: RPCExec,
		Args: payload,
	}); err != nil {
		return err
	}

	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		buf := make([]byte, 4096)
		for {
			select {
			case <-conn.Ctx().Done():
				return
			default:
			}

			n, err := os.Stdin.Read(buf)
			if n > 0 {
				_ = conn.Send(P.InputMessage{Data: buf[:n]})
			}
			if err != nil {
				return
			}
		}
	}()

	var response *P.RPCResponseMessage

loop:
	for {
		select {
		case <-conn.Ctx().Done():
			break loop
		case packet := <-conn.Receive():
			if packet.Error != nil {
				err = packet.Error
				break loop
			}

			switch msg := packet.Contents.(type) {
			case *P.OutputMessage:
				if msg.Stderr {
					_, _ = os.Stderr.Write(msg.Data)
				} else {
					_, _ = os.Stdout.Write(msg.Data)
				}
			case *P.RPCResponseMessage:
				response = msg
				break loop
			}
		}
	}

	_ = conn.Close()
	wg.Wait()

	if err != nil {
		return err
	}

	if response == nil {
		return fmt.Errorf("connection closed by server")
	}

	if response.Errored {
		return fmt.Errorf(response.Error)
	}

	execResponse := RPCExecResponse{}
	dec := codec.NewDecoderBytes(
		response.Response,
		new(codec.MsgpackHandle),
	)
	if err := dec.Decode(&execResponse); err != nil {
		return err
	}

	if len(execResponse.Data) == 0 {
		return nil
	}

	_, err = os.Stdout.Write(execResponse.Data)
	return err
}
