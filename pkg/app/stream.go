package app

import (
	"io"

	"github.com/cfoust/cy/pkg/geom"
)

type Stream struct {
	r *io.PipeReader
	w *io.PipeWriter
}

var _ App = (*Stream)(nil)

// Return the handle that allows you to write to this stream.
func (s *Stream) Writer() io.Writer {
	return s.w
}

// Resizing does nothing to a stream.
func (s *Stream) Resize(size geom.Size) error {
	return nil
}

func (s *Stream) Write(data []byte) (n int, err error) {
	return 0, nil
}

func (s *Stream) Read(p []byte) (n int, err error) {
	return s.r.Read(p)
}

func NewStream() *Stream {
	r, w := io.Pipe()
	return &Stream{
		r: r,
		w: w,
	}
}
