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

func TestSplitBracketedPaste(t *testing.T) {
	engine := NewEngine[int]()
	go engine.Poll(context.Background())
	engine.SetScopes(NewScope[int](nil))

	// Drain the initial PartialEvent from SetScopes
	<-engine.Recv()

	// Send the start marker and paste content without the end
	go engine.Input([]byte("\x1b[200~hello"))

	// Give Input time to buffer (it should not emit any events)
	time.Sleep(50 * time.Millisecond)
	select {
	case ev := <-engine.Recv():
		t.Fatalf("expected no event, got %T: %v", ev, ev)
	default:
	}

	// Now send the rest with the end marker
	go engine.Input([]byte(" world\x1b[201~"))

	// clearState emits a PartialEvent before the key
	<-engine.Recv()

	// We should get the paste event
	ev := <-engine.Recv()
	msg, ok := ev.(taro.KittyKeyMsg)
	assert.True(t, ok, "expected KittyKeyMsg, got %T", ev)
	assert.Equal(t, "hello world", msg.Text)
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
