package bind

import (
	"github.com/cfoust/cy/pkg/bind/trie"
	"github.com/cfoust/cy/pkg/janet"
)

type Action struct {
	// An arbitrary string tag stored alongside the binding.
	// Used just for docs, for now.
	Tag string
	// The Janet callback invoked when this binding is fired.
	Callback *janet.Function
}

type BindScope = trie.Trie[Action]
type BindLeaf = trie.Leaf[Action]
type BindEvent = ActionEvent[Action]

func NewBindScope() *BindScope {
	return NewScope[Action]()
}
