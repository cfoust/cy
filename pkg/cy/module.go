package cy

import (
	"github.com/cfoust/cy/pkg/wm"

	"github.com/sasha-s/go-deadlock"
)

type Cy struct {
	deadlock.RWMutex

	// The tree of groups and panes.
	tree    *wm.Node
	clients []*Client
}
