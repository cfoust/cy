package bind

import (
	"context"
	"testing"

	"github.com/cfoust/cy/pkg/bind/parse"
	"github.com/stretchr/testify/assert"
)

func sendKeys(client *Engine[int], keys ...interface{}) {
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
		client.processKey(context.Background(), input{
			Message: msg,
			Data:    []byte{},
		})
	}
}

func TestIdle(t *testing.T) {
	engine := NewEngine[int]()
	go engine.Poll(context.Background())

	scope := NewScope[int]()
	scope.Set(
		[]string{"ctrl+a"},
		2,
	)

	engine.SetScopes(scope)

	go sendKeys(
		engine,
		parse.KeyCtrlA,
	)

	<-engine.Recv()
	<-engine.Recv()
	event := <-engine.Recv()
	assert.Equal(t, ActionEvent[int]{
		Action: 2,
		Source: scope,
	}, event)
}
