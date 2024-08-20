package main

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
)

var (
	// Full references contain everything necessry to refer to a command
	// uniquely, including which cy server it's on. They can be used outside of
	// cy.
	FULL_REFERENCE = regexp.MustCompile("^(?P<socket>\\w+):(?P<node>\\d+):(?P<index>-?\\d+)$")
	// The latter two reference types are used within cy; the socket (and the
	// node) are derived from the environment.
	ABSOLUTE_REFERENCE = regexp.MustCompile("^(?P<node>\\d+):(?P<index>-?\\d+)$")
	RELATIVE_REFERENCE = regexp.MustCompile("^(?P<index>-?\\d+)$")
)

type Reference struct {
	Socket string
	Node   int
	Index  int
}

// parseReference interprets a reference string and returns a normalized
// Reference.
func parseReference(value string) (*Reference, error) {
	if match := FULL_REFERENCE.FindStringSubmatch(value); match != nil {
		node, err := strconv.Atoi(match[FULL_REFERENCE.SubexpIndex("node")])
		if err != nil {
			return nil, err
		}

		index, err := strconv.Atoi(match[FULL_REFERENCE.SubexpIndex("index")])
		if err != nil {
			return nil, err
		}

		return &Reference{
			Socket: match[FULL_REFERENCE.SubexpIndex("socket")],
			Node:   node,
			Index:  index,
		}, nil
	}

	// Need context for everything else
	socket, id, ok := getContext()
	if !ok {
		return nil, fmt.Errorf("no cy context available")
	}

	if match := ABSOLUTE_REFERENCE.FindStringSubmatch(value); match != nil {
		node, err := strconv.Atoi(match[ABSOLUTE_REFERENCE.SubexpIndex("node")])
		if err != nil {
			return nil, err
		}

		index, err := strconv.Atoi(match[ABSOLUTE_REFERENCE.SubexpIndex("index")])
		if err != nil {
			return nil, err
		}

		return &Reference{
			Socket: socket,
			Node:   node,
			Index:  index,
		}, nil
	}

	if match := RELATIVE_REFERENCE.FindStringSubmatch(value); match != nil {
		index, err := strconv.Atoi(match[RELATIVE_REFERENCE.SubexpIndex("index")])
		if err != nil {
			return nil, err
		}

		return &Reference{
			Socket: socket,
			Node:   id,
			Index:  index,
		}, nil
	}

	return nil, fmt.Errorf("invalid reference: %s", value)
}

func outputCommand(reference string) error {
	ref, err := parseReference(reference)
	if err != nil {
		return err
	}

	socketName := ref.Socket
	if CLI.Socket != "default" {
		socketName = CLI.Socket
	}

	socketPath, err := getSocketPath(socketName)
	if err != nil {
		return err
	}

	var conn Connection
	conn, err = connect(socketPath, false)
	if err != nil {
		return err
	}

	response, err := RPC[RPCOutputArgs, RPCOutputResponse](
		conn,
		RPCOutput,
		RPCOutputArgs{
			Node:  ref.Node,
			Index: ref.Index,
		},
	)
	if err != nil {
		return err
	}

	if len(response.Data) == 0 {
		return nil
	}

	_, err = os.Stdout.Write(response.Data)
	return err
}
