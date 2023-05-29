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

func (e *EchoServer) HandleClient(client Client) {
	client.Send([]byte("test"))
}

var _ Server = (*EchoServer)(nil)

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
		Serve(ctx, socketPath, &echo)
	}()

	ok := make(chan bool, 1)
	c, err := Connect(ctx, socketPath)
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
