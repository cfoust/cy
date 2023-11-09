package tree

import (
	"github.com/cfoust/cy/pkg/events"
)

type NodeEvent struct {
	Id    NodeID
	Event events.Msg
}
