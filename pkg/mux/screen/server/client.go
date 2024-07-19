package server

import (
	"context"

	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type Client struct {
	deadlock.RWMutex
	*mux.UpdatePublisher

	server     *Server
	size       mux.Size
	screen     mux.Screen
	attachment *util.Lifetime
}

var _ mux.Screen = (*Client)(nil)

func (c *Client) State() *tty.State {
	c.RLock()
	screen := c.screen
	size := c.size
	defer c.RUnlock()

	if screen == nil {
		return tty.New(size)
	}

	state := screen.State()
	stateSize := state.Image.Size()
	if stateSize.R >= size.R && stateSize.C >= size.C {
		return state
	}

	centered := tty.New(size)
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			centered.Image[row][col].Char = '-'
			centered.Image[row][col].FG = 8
		}
	}

	tty.Copy(
		size.Center(stateSize),
		centered,
		state,
	)

	return centered
}

func (c *Client) Attachment() *util.Lifetime {
	return c.attachment
}

func (c *Client) Send(msg mux.Msg) {
	c.screen.Send(msg)
}

func (c *Client) Kill() {
	c.Lock()
	if c.attachment != nil {
		c.attachment.Cancel()
	}
	c.attachment = nil
	c.screen = nil
	c.Unlock()
}

func (c *Client) Resize(size mux.Size) error {
	c.Lock()
	c.size = size
	screen := c.screen
	c.Unlock()

	if screen == nil {
		return nil
	}

	c.server.refreshPane(screen)
	return nil
}

func (c *Client) pollScreen(ctx context.Context, screen mux.Screen) error {
	subscriber := screen.Subscribe(ctx)
	changes := subscriber.Recv()

	for {
		select {
		case <-ctx.Done():
			return nil
		case event := <-changes:
			c.Publish(event)
		}
	}
}

func (c *Client) Attach(ctx context.Context, screen mux.Screen) {
	attachment := util.NewLifetime(ctx)

	c.Lock()
	if c.attachment != nil {
		c.attachment.Cancel()
	}
	c.attachment = &attachment
	oldScreen := c.screen
	c.screen = screen
	c.Unlock()

	if oldScreen != nil {
		c.server.refreshPane(oldScreen)
	}
	c.server.refreshPane(screen)

	go c.pollScreen(attachment.Ctx(), screen)

	c.Notify()
}

func (c *Client) Screen() mux.Screen {
	c.RLock()
	defer c.RUnlock()
	return c.screen
}

func (c *Client) Size() mux.Size {
	c.RLock()
	defer c.RUnlock()
	return c.size
}

func (s *Server) AddClient(
	ctx context.Context,
	initialSize mux.Size,
) *Client {
	s.Lock()
	client := &Client{
		size:            initialSize,
		UpdatePublisher: mux.NewPublisher(),
		server:          s,
	}
	s.clients = append(s.clients, client)
	s.Unlock()

	go func() {
		<-ctx.Done()
		s.Lock()
		newClients := make([]*Client, 0)
		for _, other := range s.clients {
			if client == other {
				continue
			}

			newClients = append(newClients, other)
		}
		s.clients = newClients
		s.Unlock()
	}()

	return client
}
