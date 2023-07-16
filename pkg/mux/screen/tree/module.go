package tree

import (
	"github.com/cfoust/cy/pkg/bind/trie"
	"github.com/cfoust/cy/pkg/janet"
)

type Binding struct {
	Description string
	Callback    *janet.Function
}

type BindScope = trie.Trie[Binding]
