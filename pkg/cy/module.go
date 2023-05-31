package cy

import (
	"github.com/cfoust/cy/pkg/io/pipe"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"
	"github.com/cfoust/cy/pkg/wm"

	"github.com/sasha-s/go-deadlock"
)

type Client struct {
	conn     ws.Client
	location *wm.Node
}

type Cy struct {
	deadlock.RWMutex

	// The tree of groups and panes.
	tree    *wm.Node
	clients []*Client
}

func (c *Cy) HandleClient(client ws.Client) {
	events := pipe.Map[[]byte](
		client,
		P.Encode,
		P.Decode,
	)

	events.Send(P.SizeMessage{})

	<-client.Ctx().Done()
}

var _ ws.Server = (*Cy)(nil)
