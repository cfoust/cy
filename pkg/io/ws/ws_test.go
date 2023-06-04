package ws

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

type EchoServer struct{}

func (e *EchoServer) HandleWSClient(client Client[[]byte]) {
	client.Send([]byte("test"))
}

var _ Server[[]byte] = (*EchoServer)(nil)

func TestServer(t *testing.T) {
	dir, err := os.MkdirTemp("", "example")
	if err != nil {
		t.Fail()
		return
	}

	defer os.RemoveAll(dir)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	socketPath := filepath.Join(dir, "socket")

	go func() {
		echo := EchoServer{}
		Serve[[]byte](ctx, socketPath, RawProtocol, &echo)
	}()

	ok := make(chan bool, 1)
	c, err := Connect(ctx, RawProtocol, socketPath)
	assert.NoError(t, err)
	go func() {
		time.Sleep(100 * time.Millisecond)
		reads := c.Receive()

		timeout, cancel := context.WithTimeout(ctx, 1*time.Second)
		defer cancel()

		select {
		case <-timeout.Done():
			ok <- false
		case msg := <-reads:
			ok <- string(msg.Contents) == "test"
		}
	}()

	result := <-ok
	if !result {
		t.Fail()
	}
}
