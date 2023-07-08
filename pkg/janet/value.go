package janet

/*
#cgo CFLAGS: -std=c99
#cgo LDFLAGS: -lm -ldl

#include <janet.h>
#include <api.h>
*/
import "C"

import (
	"fmt"

	"github.com/sasha-s/go-deadlock"
)

var ERROR_FREED = fmt.Errorf("cannot use freed value")

type Value struct {
	deadlock.RWMutex

	vm       *VM
	janet    C.Janet
	wasFreed bool
}

func (v *VM) value(janet C.Janet) *Value {
	C.janet_gcroot(janet)
	return &Value{
		vm:    v,
		janet: janet,
	}
}

type UnlockRequest RPC[*Value, error]

func (v *Value) IsFree() bool {
	v.RLock()
	defer v.RUnlock()
	return v.wasFreed
}

func (v *Value) unroot() {
	C.janet_gcunroot(v.janet)
}

func (v *Value) Free() {
	v.Lock()
	defer v.Unlock()
	if v.wasFreed {
		return
	}
	v.wasFreed = true

	v.vm.requests <- UnlockRequest{
		Params: v,
	}
}

func (v *Value) Unmarshal(dest interface{}) error {
	if v.IsFree() {
		return ERROR_FREED
	}

	return v.vm.unmarshal(v.janet, dest)
}

type Table struct {
	*Value
	table *C.JanetTable
}

type Function struct {
	*Value
	function *C.JanetFunction
}
