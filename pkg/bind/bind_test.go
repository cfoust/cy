package bind

import (
	"context"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/taro"

	"github.com/stretchr/testify/assert"
)

func sendKeys(client *Engine[int], keys ...string) {
	msgs := taro.KeysToMsg(keys...)

	for _, msg := range msgs {
		client.processKey(context.Background(), msg)
	}
}

func TestAction(t *testing.T) {
	engine := NewEngine[int]()
	go engine.Poll(context.Background())

	scope := NewScope[int](nil)
	scope.Set(
		[]interface{}{"ctrl+a"},
		2,
	)

	engine.SetScopes(scope)

	go sendKeys(
		engine,
		"ctrl+a",
	)

	<-engine.Recv()
	<-engine.Recv()
	event := <-engine.Recv()
	assert.Equal(t, ActionEvent[int]{
		Engine:   engine,
		Action:   2,
		Source:   scope,
		Sequence: []string{"ctrl+a"},
	}, event)
}

func TestIdle(t *testing.T) {
	engine := NewEngine[int]()

	go engine.Poll(context.Background())

	go func() {
		for {
			<-engine.Recv()
		}
	}()

	scope := NewScope[int](nil)
	scope.Set(
		[]interface{}{"ctrl+a", "a"},
		2,
	)

	engine.SetScopes(scope)

	sendKeys(
		engine,
		"ctrl+a",
	)

	assert.Equal(t, []string{
		"ctrl+a",
	}, engine.getState())

	time.Sleep(time.Second + 50*time.Millisecond)

	assert.Equal(t, engine.getState(), []string{})
}
