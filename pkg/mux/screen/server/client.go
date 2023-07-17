package server

import (
	"context"

	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
	"github.com/xo/terminfo"
)

type Client struct {
	deadlock.RWMutex
	server *Server

	size       mux.Size
	screen     mux.Screen
	attachment *util.Lifetime
	publisher  *mux.UpdatePublisher
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

	return screen.State()
}

func (c *Client) Updates() *mux.Updater {
	return c.publisher.Subscribe()
}

func (c *Client) Write(data []byte) (n int, err error) {
	return c.screen.Write(data)
}

func (c *Client) Resize(size mux.Size) error {
	c.Lock()
	c.size = size
	screen := c.screen
	c.Unlock()

	c.server.refreshPane(screen)
	return nil
}

func (c *Client) pollScreen(ctx context.Context, screen mux.Screen) error {
	subscriber := screen.Updates()
	defer subscriber.Done()

	changes := subscriber.Recv()

	for {
		select {
		case <-ctx.Done():
			return nil
		case state := <-changes:
			c.publisher.Publish(state)
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

	c.publisher.Publish(c.State())
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
	info *terminfo.Terminfo,
	initialSize mux.Size,
) *Client {
	s.Lock()
	client := &Client{
		size:      initialSize,
		publisher: mux.NewPublisher(),
		server:    s,
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
