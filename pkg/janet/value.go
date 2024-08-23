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
	isSafe   bool
}

func (v *VM) value(janet C.Janet) *Value {
	C.janet_gcroot(janet)
	return &Value{
		vm:     v,
		janet:  janet,
		isSafe: v.isSafe,
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

func (v *Value) JSON() ([]byte, error) {
	if v.IsFree() {
		return nil, ERROR_FREED
	}

	out, err := v.vm.jsonEncode.CallResult(
		context.Background(),
		nil,
		v,
	)
	if err != nil {
		return nil, err
	}

	defer out.Free()

	var result []byte
	err = out.Unmarshal(&result)
	if err != nil {
		return nil, err
	}

	return result, nil
}

func (v *Value) Raw() ([]byte, error) {
	if v.IsFree() {
		return nil, ERROR_FREED
	}

	out, err := v.vm.raw.CallResult(
		context.Background(),
		nil,
		v,
	)
	if err != nil {
		return nil, err
	}

	defer out.Free()

	var str string
	if err := out.Unmarshal(&str); err == nil {
		return []byte(str), nil
	}

	var result []byte
	err = out.Unmarshal(&result)
	if err != nil {
		return nil, err
	}

	return result, nil
}

func (v *Value) String() string {
	if v.IsFree() {
		return ""
	}

	out, err := v.vm.format.CallResult(
		context.Background(),
		nil,
		"%j",
		v,
	)
	if err != nil {
		return ""
	}

	var result string
	err = out.Unmarshal(&result)
	if err != nil {
		out.Free()
		return ""
	}

	return result
}

type unmarshalRequest struct {
	source C.Janet
	dest   interface{}
	errc   chan error
}

type marshalRequest struct {
	source interface{}
	result chan interface{}
}

func (v *Value) Unmarshal(dest interface{}) error {
	v.RLock()
	isSafe := v.isSafe
	v.RUnlock()

	if isSafe {
		return v.vm.unmarshal(v.janet, dest)
	}

	if v.IsFree() {
		return ERROR_FREED
	}

	return v.vm.unmarshalSafe(v.janet, dest)
}

func (v *Value) Nil() bool {
	// XXX add real support for checking nil, but this works
	var val *int = nil
	return v.Unmarshal(&val) == nil
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

	return req.WaitErr()
}

func (f *Function) CallResult(
	ctx context.Context,
	user interface{},
	params ...interface{},
) (*Value, error) {
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

	return req.WaitOut()
}

func (f *Function) Call(ctx context.Context, params ...interface{}) error {
	return f.CallContext(ctx, nil, params...)
}
