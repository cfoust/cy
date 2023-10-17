package bind

import (
	"github.com/cfoust/cy/pkg/bind/trie"
	"github.com/cfoust/cy/pkg/janet"
)

type Action struct {
	Description string
	Callback    *janet.Function
}

type BindScope = trie.Trie[Action]
type BindEvent = ActionEvent[Action]

func NewBindScope() *BindScope {
	return NewScope[Action]()
}
