package protocol

import (
	"bytes"
	"fmt"

	"github.com/cfoust/cy/pkg/io/ws"

	"github.com/ugorji/go/codec"
)

func Decode(data []byte) (Message, error) {
	handle := new(codec.MsgpackHandle)
	decoder := codec.NewDecoderBytes(data, handle)

	var type_ MessageType
	err := decoder.Decode(&type_)
	if err != nil {
		return nil, err
	}

	var msg Message = nil
	switch type_ {
	case MessageTypeHandshake:
		msg = &HandshakeMessage{}
	case MessageTypeError:
		msg = &ErrorMessage{}
	case MessageTypeInput:
		msg = &InputMessage{}
	case MessageTypeOutput:
		msg = &OutputMessage{}
	case MessageTypeSize:
		msg = &SizeMessage{}
	case MessageTypeClose:
		msg = &CloseMessage{}
	case MessageTypeRPCRequest:
		msg = &RPCRequestMessage{}
	case MessageTypeRPCResponse:
		msg = &RPCResponseMessage{}
	default:
		return nil, fmt.Errorf("invalid type: %d", type_)
	}

	err = decoder.Decode(msg)
	if err != nil {
		return nil, err
	}

	return msg, nil
}

func Encode(message Message) ([]byte, error) {
	handle := new(codec.MsgpackHandle)
	result := new(bytes.Buffer)
	encoder := codec.NewEncoder(result, handle)

	err := encoder.Encode(message.Type())
	if err != nil {
		return nil, err
	}

	err = encoder.Encode(message)
	if err != nil {
		return nil, err
	}

	return result.Bytes(), nil
}

type protocol struct{}

func (p protocol) Encode(message Message) ([]byte, error) {
	return Encode(message)
}

func (p protocol) Decode(data []byte) (Message, error) {
	return Decode(data)
}

var Protocol ws.Protocol[Message] = protocol{}
