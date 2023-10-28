package tree

// ReplayEvent is an event triggered by a binding in the :replay scope.
type ReplayEvent struct {
	Id    NodeID
	Event interface{}
}
