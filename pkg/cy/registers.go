package cy

import (
	"github.com/cfoust/cy/pkg/clipboard"
	"github.com/cfoust/cy/pkg/cy/api"

	"github.com/sasha-s/go-deadlock"
)

type memoryRegisters struct {
	deadlock.RWMutex
	clipboard clipboard.Clipboard
	registers map[string]string
}

var _ api.Registers = (*memoryRegisters)(nil)

func (r *memoryRegisters) Set(register string, text string) error {
	if register == "+" {
		return r.clipboard.Write(text)
	}

	r.Lock()
	defer r.Unlock()
	r.registers[register] = text
	return nil
}

func (r *memoryRegisters) Get(register string) (string, error) {
	if register == "+" {
		return r.clipboard.Read()
	}

	r.RLock()
	defer r.RUnlock()
	return r.registers[register], nil
}

func (r *memoryRegisters) GetAll() (map[string]string, error) {
	r.RLock()
	defer r.RUnlock()
	cloned := make(map[string]string)

	for k, v := range r.registers {
		cloned[k] = v
	}

	// Attempt to get system clipboard, but don't worry if we can't
	clipboard, err := r.clipboard.Read()
	if err == nil {
		cloned["+"] = clipboard
	}

	return cloned, nil
}

func newMemoryRegisters(clipboard clipboard.Clipboard) *memoryRegisters {
	return &memoryRegisters{
		registers: make(map[string]string),
		clipboard: clipboard,
	}
}
