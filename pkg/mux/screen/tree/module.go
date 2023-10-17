package tree

import (
	"github.com/cfoust/cy/pkg/bind"
)

// ReplayEvent is an event triggered by a binding in the :replay scope.
type ReplayEvent struct {
	Id    NodeID
	Event bind.BindEvent
}
