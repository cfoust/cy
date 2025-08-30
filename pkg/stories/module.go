package stories

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

type Config struct {
	// If zero, the story will resize
	Size geom.Size
	// If true, the viewer captures the screen immediately and uses that
	// instead of a live view.
	IsSnapshot bool
	// A list of inputs that will be executed in order when the story
	// begins.
	Input []interface{}
}

func (c *Config) HasInputs() bool {
	return len(c.Input) > 0
}

type WaitEvent struct {
	Duration time.Duration
}

const (
	ABit = 300 * time.Millisecond
	Some = time.Second
	More = 3 * time.Second
	ALot = 5 * time.Second
)

func Wait(duration time.Duration) WaitEvent {
	return WaitEvent{Duration: duration}
}

const TYPE_DELAY = 100 * time.Millisecond

// Simulate typing.
func Type(msgs ...string) (result []interface{}) {
	for _, key := range taro.KeysToMsg(msgs...) {
		runes := key.Runes
		if len(runes) == 0 {
			result = append(result, key, Wait(TYPE_DELAY))
			continue
		}

		for _, r := range runes {
			newKey := key
			newKey.Runes = []rune{r}
			result = append(
				result,
				taro.KeyMsg(newKey),
				Wait(TYPE_DELAY),
			)
		}
	}

	return
}

type InitFunc func(context.Context) (mux.Screen, error)

type Story struct {
	Name   string
	Init   InitFunc
	Config Config
}

var Stories = make(map[string]Story)

func Register(name string, init InitFunc, config Config) {
	Stories[name] = Story{
		Name:   name,
		Init:   init,
		Config: config,
	}
}

func Send(s mux.Screen, msgs ...interface{}) {
	var realMsg tea.Msg
	for _, msg := range msgs {
		realMsg = msg
		switch msg := msg.(type) {
		case string:
			keyMsgs := taro.KeysToMsg(msg)
			if len(keyMsgs) == 1 {
				realMsg = keyMsgs[0]
			}
		}
		s.Send(realMsg)
	}
}
