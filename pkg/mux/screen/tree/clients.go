package tree

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen"

	"github.com/sasha-s/go-deadlock"
)

type Client struct {
	deadlock.RWMutex
	node   Node
	binds  *bind.Engine[Binding]
	screen mux.Screen
}

func (t *Tree) AddClient(ctx context.Context, initialSize geom.Size) mux.Screen {
	t.Lock()
	client := &Client{
		binds: bind.NewEngine[Binding](),
		screen: screen.NewTerminal(
			ctx,
			nil,
			initialSize,
		),
	}
	t.clients = append(t.clients, client)
	t.Unlock()

	go t.pollClient(client)

	return client.screen
}
