package params

import (
	"github.com/sasha-s/go-deadlock"
)

type Parameters struct {
	deadlock.RWMutex
	parent *Parameters
	table  map[string]interface{}
}

func (p *Parameters) set(key string, value interface{}) error {
	p.Lock()
	p.table[key] = value
	p.Unlock()
	return nil
}

func (p *Parameters) Set(key string, value interface{}) error {
	if p.isDefault(key) {
		return p.setDefault(key, value)
	}

	return p.set(key, value)
}

func (p *Parameters) Get(key string) (value interface{}, ok bool) {
	var current, parent *Parameters
	current = p
	for current != nil {
		current.RLock()
		value, ok = current.table[key]
		parent = current.parent
		current.RUnlock()
		if ok {
			return value, ok
		}
		current = parent
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
