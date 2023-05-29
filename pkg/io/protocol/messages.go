package protocol

type MessageType int

const (
	MessageTypeInput MessageType = iota
	MessageTypeOutput
	MessageTypeSize
)

type Message interface {
	Type() MessageType
}

type DataMessage struct {
	Data []byte
}

type InputMessage DataMessage

func (i InputMessage) Type() MessageType { return MessageTypeInput }

type OutputMessage DataMessage

func (i OutputMessage) Type() MessageType { return MessageTypeOutput }

type SizeMessage struct {
	Width  int
	Height int
}

func (i SizeMessage) Type() MessageType { return MessageTypeSize }
