package bind

import (
	"context"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/bind/parse"
	"github.com/stretchr/testify/assert"
)

func sendKeys(client *Client[int], keys ...interface{}) {
	msgs := make([]parse.KeyMsg, 0)

	for _, key := range keys {
		switch key := key.(type) {
		case string:
			for _, char := range key {
				msgs = append(msgs, parse.KeyMsg{
					Type:  parse.KeyRunes,
					Runes: []rune{char},
				})
			}
		case parse.KeyType:
			msgs = append(msgs, parse.KeyMsg{
				Type: key,
			})
		}
	}

	for _, msg := range msgs {
		client.process(msg, []byte{})
	}
}

func TestIdle(t *testing.T) {
	engine := NewEngine[int](context.Background())
	client := engine.AddClient()

	go func() {
		for {
			<-client.Recv()
		}
	}()

	scope := NewScope[int]()

	root := engine.Root()
	root.Bind(
		"ctrl+a",
		Result[int]{
			Scope: scope,
		},
	)

	sendKeys(
		client,
		parse.KeyCtrlA,
	)
	assert.Equal(t, client.Scope(), scope)

	time.Sleep(250 * time.Millisecond)
	assert.Equal(t, client.Scope(), root)
}
