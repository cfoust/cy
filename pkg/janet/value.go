package janet

/*
#cgo CFLAGS: -std=c99
#cgo LDFLAGS: -lm -ldl

#include <janet.h>
#include <api.h>
*/
import "C"

import (
	"context"
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

type unlockRequest struct {
	Value *Value
}

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

	v.vm.requests <- unlockRequest{
		Value: v,
	}
}

type unmarshalRequest struct {
	source C.Janet
	dest   interface{}
	errc   chan error
}

func (v *Value) Unmarshal(dest interface{}) error {
	if v.IsFree() {
		return ERROR_FREED
	}

	return v.vm.Unmarshal(v.janet, dest)
}

type Table struct {
	*Value
	table *C.JanetTable
}

type Fiber struct {
	*Value
	fiber *C.JanetFiber
}

type Keyword string

type Function struct {
	*Value
	function *C.JanetFunction
}

type functionRequest struct {
	Params
	Args     []interface{}
	Function *Function
}

func (f *Function) CallContext(
	ctx context.Context,
	user interface{},
	params ...interface{},
) error {
	result := make(chan Result)
	req := functionRequest{
		Args:     params,
		Function: f,
		Params: Params{
			Context: ctx,
			User:    user,
			Result:  result,
		},
	}
	f.vm.requests <- req

	return req.Wait()
}

func (f *Function) Call(ctx context.Context, params ...interface{}) error {
	return f.CallContext(ctx, nil, params...)
}
