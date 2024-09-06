package sessions

import (
	"compress/gzip"
	"fmt"
	"os"

	P "github.com/cfoust/cy/pkg/io/protocol"

	"github.com/ugorji/go/codec"
)

const (
	SESSION_FILE_VERSION = 1
)

type header struct {
	Version int
}

type SessionWriter interface {
	Write(event Event) error
	Close() error
}

type sessionWriter struct {
	file    *os.File
	gz      *gzip.Writer
	handle  *codec.MsgpackHandle
	encoder *codec.Encoder
}

func (s *sessionWriter) Write(event Event) error {
	if err := s.encoder.Encode(event.Stamp); err != nil {
		return err
	}

	data := event.Message
	if err := s.encoder.Encode(data.Type()); err != nil {
		return err
	}

	switch msg := data.(type) {
	case P.OutputMessage:
		// slight optimization--we don't need to encode the field name
		// every time
		return s.encoder.Encode(msg.Data)
	case P.SizeMessage:
		return s.encoder.Encode(msg)

	default:
		return fmt.Errorf("cannot encode unimplemented message type: %+v", msg)
	}
}

func (s *sessionWriter) Close() error {
	if err := s.gz.Flush(); err != nil {
		return err
	}

	if err := s.gz.Close(); err != nil {
		return err
	}

	return s.file.Close()
}

func Create(filename string) (SessionWriter, error) {
	f, err := os.Create(filename)
	if err != nil {
		return nil, err
	}

	handle := new(codec.MsgpackHandle)
	gz := gzip.NewWriter(f)
	encoder := codec.NewEncoder(gz, handle)

	writer := sessionWriter{
		handle:  handle,
		gz:      gz,
		encoder: encoder,
		file:    f,
	}

	if err := encoder.Encode(header{
		Version: SESSION_FILE_VERSION,
	}); err != nil {
		return nil, err
	}

	if err := gz.Flush(); err != nil {
		return nil, err
	}

	return &writer, nil
}

type SessionReader interface {
	Read() (Event, error)
}

type sessionReader struct {
	file    *os.File
	gz      *gzip.Reader
	handle  *codec.MsgpackHandle
	decoder *codec.Decoder
}

func (s *sessionReader) Read() (Event, error) {
	event := Event{}

	err := s.decoder.Decode(&event.Stamp)
	if err != nil {
		return event, err
	}

	var type_ P.MessageType
	err = s.decoder.Decode(&type_)
	if err != nil {
		return event, err
	}

	var msg P.Message
	switch type_ {
	case P.MessageTypeOutput:
		var data []byte
		if err := s.decoder.Decode(&data); err != nil {
			return event, err
		}
		msg = P.OutputMessage{
			Data: data,
		}
	case P.MessageTypeSize:
		size := P.SizeMessage{}
		if err := s.decoder.Decode(&size); err != nil {
			return event, err
		}
		msg = size
	}

	event.Message = msg
	return event, nil
}

func Open(filename string) (SessionReader, error) {
	f, err := os.Open(filename)
	if err != nil {
		return nil, err
	}

	handle := new(codec.MsgpackHandle)
	gz, err := gzip.NewReader(f)
	if err != nil {
		return nil, fmt.Errorf("gzip error: %w", err)
	}
	decoder := codec.NewDecoder(gz, handle)

	reader := sessionReader{
		handle:  handle,
		gz:      gz,
		decoder: decoder,
		file:    f,
	}

	var h header
	err = decoder.Decode(&h)
	if err != nil {
		return nil, fmt.Errorf("could not decode header: %w", err)
	}

	if h.Version != SESSION_FILE_VERSION {
		return nil, fmt.Errorf("header version %d did not match %d", h.Version, SESSION_FILE_VERSION)
	}

	return &reader, nil
}
