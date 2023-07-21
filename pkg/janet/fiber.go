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

type FiberRequest struct {
	Params
	// The fiber to run
	Fiber Fiber
	// The value with which to resume
	Value *Value
}

func (v *VM) createFiber(fun *C.JanetFunction, args []C.Janet) Fiber {
	fiber := C.janet_fiber(
		fun,
		64,
		C.int(len(args)),
		(*C.Janet)(unsafe.Pointer(&args[0])),
	)

	return Fiber{
		Value: v.value(C.janet_wrap_fiber(fiber)),
		fiber: fiber,
	}
}

// Run a fiber to completion.
func (v *VM) runFiber(context Params, fiber Fiber, in *Value) {
	v.requests <- FiberRequest{
		Params: context,
		Fiber:  fiber,
		Value:  in,
	}
}

func (v *VM) handleYield(fiber *C.JanetFiber, value C.Janet, errc chan error) {
	if C.janet_checktype(value, C.JANET_ARRAY) == 0 {
		errc <- fmt.Errorf("(yield) called with non-array value")
		return
	}

	// We don't want any of the arguments to get garbage collected
	// before we're done executing
	C.janet_gcroot(value)

	args := make([]C.Janet, 0)
	for i := 0; i < int(C.janet_length(value)); i++ {
		args = append(
			args,
			C.janet_get(
				value,
				C.janet_wrap_integer(C.int(i)),
			),
		)
	}

	// go run the callback in a new goroutine
	go func() {
		defer C.janet_gcunroot(value)

		// TODO(cfoust): 07/20/23 ctx
		result, err := v.executeCallback(args)

		if err != nil {
			errc <- err
			return
		}

		// send a new event
		v.requests <- FiberRequest{
			Params: Params{
				User:   v.user,
				Result: errc,
			},
			Fiber: Fiber{
				Value: v.value(
					C.janet_wrap_fiber(fiber),
				),
				fiber: fiber,
			},
			Value: v.value(result),
		}
	}()

	return
}
