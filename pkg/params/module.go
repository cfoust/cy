package params

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"

	"github.com/sasha-s/go-deadlock"
)

type Parameters struct {
	deadlock.RWMutex
	parent *Parameters
	table  map[string]interface{}
}

func (p *Parameters) Set(key string, value interface{}) error {
	if !janet.IsValidType(value) {
		return fmt.Errorf("all parameters must be representable in Janet")
	}

	p.Lock()
	p.table[key] = value
	p.Unlock()
	return nil
}

func (p *Parameters) Get(key string) (value interface{}, ok bool) {
	var current *Parameters = p
	for current != nil {
		current.RLock()
		value, ok = current.table[key]
		current.RUnlock()
		if ok {
			return value, ok
		}
		current = current.parent
	}

	return nil, false
}

func (p *Parameters) NewChild() *Parameters {
	child := New()
	child.parent = p
	return child
}

func (p *Parameters) SetParent(parent *Parameters) {
	p.Lock()
	p.parent = parent
	p.Unlock()
}

func New() *Parameters {
	return &Parameters{
		table: make(map[string]interface{}),
	}
}
