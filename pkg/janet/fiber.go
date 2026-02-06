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
	_ "embed"
	"fmt"
	"reflect"
	"unsafe"
)

// Result is the value produced when a fiber finishes executing. Fibers can halt in three ways:
// - They can return a value, which is stored in Out.
// - They can yield (with (yield)), which is stored in Yield.
// - They can error, the message for which is stored in Error.
type Result struct {
	Out   *Value
	Yield *Value
	Error error
}

type Params struct {
	Context context.Context
	User    interface{}
	Result  chan Result
	Dyns    map[Keyword]interface{}
}

// Make new Params with an overwritten Result channel.
func (p Params) Pipe() Params {
	return Params{
		Context: p.Context,
		User:    p.User,
		Result:  make(chan Result, 1),
		Dyns:    p.Dyns,
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

func (p Params) Yield(value *Value) {
	p.Result <- Result{
		Yield: value,
	}
}

// WaitErr waits for a fiber to finish executing, ignoring any values it
// returns or yields.
func (p Params) WaitErr() error {
	select {
	case result := <-p.Result:
		if result.Out != nil {
			result.Out.Free()
		}
		if result.Yield != nil {
			result.Yield.Free()
		}
		return result.Error
	case <-p.Context.Done():
		return p.Context.Err()
	}
}

// WaitOut waits for the result of a fiber. If the fiber yields (instead of
// just returning a result), WaitOut will return an error.
func (p Params) WaitOut() (*Value, error) {
	select {
	case result := <-p.Result:
		if result.Yield != nil {
			return nil, fmt.Errorf("unexpected yield")
		}

		return result.Out, result.Error
	case <-p.Context.Done():
		return nil, p.Context.Err()
	}
}

// WaitResult waits for the fiber to produce a Result and returns it.
func (p Params) WaitResult() (*Result, error) {
	select {
	case result := <-p.Result:
		return &result, result.Error
	case <-p.Context.Done():
		return nil, p.Context.Err()
	}
}

type fiberRequest struct {
	Params
	// The fiber to run
	Fiber *Fiber
	// The value with which to resume
	In *Value
}

func (v *VM) createFiber(
	fun *C.JanetFunction,
	args []C.Janet,
	dyns map[Keyword]interface{},
) (*Fiber, error) {
	argPtr := unsafe.Pointer(nil)
	if len(args) > 0 {
		argPtr = unsafe.Pointer(&args[0])
	}

	// Check arity - for variadic functions, check against min/max arity
	minArity := int(fun.def.min_arity)
	maxArity := int(fun.def.max_arity)

	if len(args) < minArity {
		return nil, fmt.Errorf(
			"function takes at least %d args, got %d",
			minArity,
			len(args),
		)
	}

	if maxArity >= 0 && len(args) > maxArity {
		return nil, fmt.Errorf(
			"function takes at most %d args, got %d",
			maxArity,
			len(args),
		)
	}

	fiber := C.janet_fiber(
		fun,
		64,
		C.int(len(args)),
		(*C.Janet)(argPtr),
	)

	// Set dynamic bindings on the fiber if provided
	if len(dyns) > 0 {
		fiber.env = C.janet_table(C.int(len(dyns)))
		for name, value := range dyns {
			marshaled, err := v.marshal(value)
			if err != nil {
				return nil, fmt.Errorf(
					"failed to marshal dyn %s: %w",
					name,
					err,
				)
			}
			key := wrapKeyword(string(name))
			C.janet_table_put(fiber.env, key, marshaled)
		}
	}

	return &Fiber{
		Value: v.value(C.janet_wrap_fiber(fiber)),
		fiber: fiber,
	}, nil
}

// Run a fiber to completion.
func (v *VM) runFiber(params Params, fiber *Fiber, in *Value) {
	v.requests <- fiberRequest{
		Params: params,
		Fiber:  fiber,
		In:     in,
	}
}

// handleCallback invokes a callback defined in Go.
func (v *VM) handleCallback(params Params, fiber *Fiber, out C.Janet) {
	if C.janet_checktype(out, C.JANET_TUPLE) == 0 {
		params.Error(fmt.Errorf("(signal) called with non-tuple"))
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
}

func (v *VM) continueFiber(params Params, fiber *Fiber, in *Value) {
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
	case C.JANET_SIGNAL_YIELD:
		params.Yield(v.value(out))
		return
	case C.JANET_SIGNAL_ERROR:
		var errStr string
		if err := v.unmarshal(out, &errStr); err != nil {
			params.Error(err)
			return
		}

		params.Error(fmt.Errorf("%s", errStr))
		return
	case C.JANET_SIGNAL_USER5:
		v.handleCallback(params, fiber, out)
		return
	default:
		params.Error(fmt.Errorf("unrecognized signal: %d", signal))
		return
	}
}
