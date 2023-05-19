package util

import (
	"io"

	"github.com/sasha-s/go-deadlock"
)

type Resizable interface {
	Resize(width, height int)
}

type WaitBuffer struct {
	deadlock.Mutex
	data   []byte
	writes chan bool
}

func NewWaitBuffer() *WaitBuffer {
	return &WaitBuffer{
		data:   make([]byte, 0),
		writes: make(chan bool, 256),
	}
}

func (w *WaitBuffer) Write(p []byte) (n int, err error) {
	w.Lock()
	defer w.Unlock()

	w.data = append(w.data, p...)

	w.writes <- true

	return len(p), nil
}

func (w *WaitBuffer) Read(p []byte) (n int, err error) {
	w.Lock()
	data := w.data
	w.Unlock()

	if len(data) == 0 {
		<-w.writes
	}

	w.Lock()
	data = w.data
	defer w.Unlock()

	numRead := len(p)
	if numRead > len(data) {
		numRead = len(data)
	}

	copy(p, data[:numRead])
	w.data = data[numRead:]

	return numRead, nil
}

var _ io.ReadWriter = (*WaitBuffer)(nil)
