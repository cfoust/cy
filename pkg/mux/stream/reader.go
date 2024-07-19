package stream

import (
	"io"

	"github.com/cfoust/cy/pkg/emu"
)

type Reader struct {
	r *io.PipeReader
	w *io.PipeWriter
}

var _ Stream = (*Reader)(nil)

func (s *Reader) Kill() {
}

// Return the handle that allows you to write to this stream.
func (s *Reader) Writer() io.Writer {
	return s.w
}

// Resizing does nothing to a stream.
func (s *Reader) Resize(size Size) error {
	return nil
}

func (s *Reader) Write(data []byte) (n int, err error) {
	return 0, nil
}

func (s *Reader) Read(p []byte) (n int, err error) {
	return s.r.Read(p)
}

func NewReader() *Reader {
	r, w := io.Pipe()

	go func() {
		// Set the terminal to CRLF mode so that carriage returns go
		// back to the first column
		w.Write([]byte(emu.LineFeedMode))
	}()

	return &Reader{
		r: r,
		w: w,
	}
}
