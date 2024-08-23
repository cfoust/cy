package search

type ActionType int

type ActionEvent struct {
	Type ActionType
}

const (
	ActionCancel ActionType = iota
	ActionNext
	ActionPrev
	ActionFirst
	ActionLast
)
