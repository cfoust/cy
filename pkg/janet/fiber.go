package janet

/*
#cgo CFLAGS: -std=c99
#cgo LDFLAGS: -lm -ldl

#include <janet.h>
#include <api.h>
*/
import "C"
import _ "embed"

import (
	"context"
	"fmt"
	"reflect"
	"unsafe"
)

type Result struct {
	Out   *Value
	Error error
}

type Params struct {
	Context context.Context
	User    interface{}
	Result  chan Result
}

// Make new Params with an overwritten Result channel.
func (p Params) Pipe() Params {
	return Params{
		Context: p.Context,
		User:    p.User,
		Result:  make(chan Result),
	}
}

func (p Params) Error(err error) {
	p.Result <- Result{
		Error: err,
	}
}

func (p Params) Ok() {
	p.Error(nil)
}

func (p Params) Out(value *Value) {
	p.Result <- Result{
		Out: value,
	}
}

func (p Params) WaitErr() error {
	select {
	case result := <-p.Result:
		if result.Out != nil {
			result.Out.Free()
		}
		return result.Error
	case <-p.Context.Done():
		return p.Context.Err()
	}
}

func (p Params) WaitResult() (*Value, error) {
	select {
	case result := <-p.Result:
		return result.Out, result.Error
	case <-p.Context.Done():
		return nil, p.Context.Err()
	}
}

type fiberRequest struct {
	Params
	// The fiber to run
	Fiber Fiber
	// The value with which to resume
	In *Value
}

func (v *VM) createFiber(fun *C.JanetFunction, args []C.Janet) Fiber {
	argPtr := unsafe.Pointer(nil)
	if len(args) > 0 {
		argPtr = unsafe.Pointer(&args[0])
	}

	fiber := C.janet_fiber(
		fun,
		64,
		C.int(len(args)),
		(*C.Janet)(argPtr),
	)

	return Fiber{
		Value: v.value(C.janet_wrap_fiber(fiber)),
		fiber: fiber,
	}
}

// Run a fiber to completion.
func (v *VM) runFiber(params Params, fiber Fiber, in *Value) {
	v.requests <- fiberRequest{
		Params: params,
		Fiber:  fiber,
		In:     in,
	}
}

func (v *VM) handleYield(params Params, fiber Fiber, out C.Janet) {
	if C.janet_checktype(out, C.JANET_TUPLE) == 0 {
		params.Error(fmt.Errorf("(yield) called with non-tuple"))
		return
	}

	args := make([]C.Janet, 0)
	for i := 0; i < int(C.janet_length(out)); i++ {
		args = append(
			args,
			C.janet_get(
				out,
				C.janet_wrap_integer(C.int(i)),
			),
		)
	}

	partial, err := v.setupCallback(params, args)
	if err != nil {
		params.Error(err)
		return
	}

	outc := make(chan []reflect.Value)
	go func() {
		outc <- partial.Call()
	}()
	go func() {
		select {
		case <-params.Context.Done():
			params.Error(params.Context.Err())
			return
		case out := <-outc:
			v.requests <- resolveRequest{
				Params: params,
				Fiber:  fiber,
				Type:   partial.Type,
				Out:    out,
			}
		}
	}()

	return
}

func (v *VM) continueFiber(params Params, fiber Fiber, in *Value) {
	arg := C.janet_wrap_nil()
	if in != nil {
		arg = in.janet
		defer in.unroot()
	}

	var out C.Janet
	signal := C.janet_continue(
		fiber.fiber,
		arg,
		&out,
	)

	switch signal {
	case C.JANET_SIGNAL_OK:
		params.Out(v.value(out))
		return
	case C.JANET_SIGNAL_ERROR:
		var errStr string
		if err := v.unmarshal(out, &errStr); err != nil {
			params.Error(err)
			return
		}

		params.Error(fmt.Errorf("%s", errStr))
		return
	case C.JANET_SIGNAL_YIELD:
		v.handleYield(params, fiber, out)
	default:
		params.Error(fmt.Errorf("unrecognized signal: %d", signal))
		return
	}
}
