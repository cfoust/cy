package protocol

import (
	"github.com/cfoust/cy/pkg/geom"

	"github.com/muesli/termenv"
)

type MessageType int

const (
	MessageTypeHandshake MessageType = iota
	MessageTypeError
	MessageTypeSize
	MessageTypeInput
	MessageTypeOutput
	MessageTypeRPCRequest
	MessageTypeRPCResponse
	MessageTypeClose
)

type Message interface {
	Type() MessageType
}

type DataMessage struct {
	Data []byte
}

// Represents client input (typically, typing.)
type InputMessage DataMessage

func (i InputMessage) Type() MessageType { return MessageTypeInput }

// Output that should be sent to the client.
type OutputMessage DataMessage

func (i OutputMessage) Type() MessageType { return MessageTypeOutput }

// A message indicating the connection is complete.
type CloseMessage struct {
	Reason string
}

func (i CloseMessage) Type() MessageType { return MessageTypeClose }

// Used when the client terminal is resized.
type SizeMessage struct {
	Rows    int
	Columns int
}

func (i SizeMessage) Type() MessageType { return MessageTypeSize }

func (i SizeMessage) Vec() geom.Vec2 {
	return geom.Vec2{R: i.Rows, C: i.Columns}
}

// The initial information necessary to render to the client.
type HandshakeMessage struct {
	Env     map[string]string
	Shell   string
	Size    geom.Vec2
	Profile termenv.Profile
}

func (i HandshakeMessage) Type() MessageType { return MessageTypeHandshake }

// Send an error to the client. Used before closing the connection.
type ErrorMessage struct {
	Message string
}

func (i ErrorMessage) Type() MessageType { return MessageTypeError }

type RPCRequestMessage struct {
	Name string
	Args []byte
}

func (r RPCRequestMessage) Type() MessageType { return MessageTypeRPCRequest }

type RPCResponseMessage struct {
	Response []byte
	Errored  bool
	Error    string
}

func (r RPCResponseMessage) Type() MessageType { return MessageTypeRPCResponse }
