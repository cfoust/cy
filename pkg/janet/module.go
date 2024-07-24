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
	"runtime"

	"github.com/sasha-s/go-deadlock"
)

//go:embed go-boot.janet
var GO_BOOT_FILE []byte

type Request interface{}

type VM struct {
	deadlock.RWMutex

	callbacks map[string]*Callback
	evaluate  C.Janet

	requests chan Request

	env *Table
}

func initJanet() {
	C.janet_init()
}

func deInitJanet() {
	C.janet_deinit()
}

func (v *VM) Env() *Table {
	return v.env
}

// Wait for code calls and process them.
func (v *VM) poll(ctx context.Context, ready chan bool) {
	// All Janet state is thread-local, so we explicitly want to execute
	// all Janet code in the same OS thread.
	runtime.LockOSThread()

	initJanet()
	defer deInitJanet()

	// Set up the core environment
	env := C.janet_core_env(nil)
	v.runCodeUnsafe(GO_BOOT_FILE, "go-boot.janet")

	// Then store our evaluation function
	var evaluate C.Janet
	C.janet_resolve(env, C.janet_csymbol(C.CString("go/evaluate")), &evaluate)
	C.janet_gcroot(evaluate)
	v.evaluate = evaluate

	ready <- true

	for {
		select {
		case <-ctx.Done():
			return
		case req := <-v.requests:
			switch req := req.(type) {
			case callRequest:
				params := req.Params
				v.runCode(params, req.Call)
			case fiberRequest:
				params := req.Params
				v.continueFiber(params, req.Fiber, req.In)
			case unlockRequest:
				req.Value.unroot()
			case functionRequest:
				params := req.Params
				v.runFunction(
					params,
					req.Function.function,
					req.Args,
				)
			case resolveRequest:
				result, err := v.resolveCallback(
					req.Type,
					req.Out,
				)

				var wrapped C.Janet
				if err != nil {
					wrapped = wrapError(err.Error())
				} else {
					wrapped = C.wrap_result_value(result)
				}

				go v.runFiber(
					req.Params,
					req.Fiber,
					v.value(wrapped),
				)
			case unmarshalRequest:
				req.errc <- v.unmarshal(
					req.source,
					req.dest,
				)
			case marshalRequest:
				value, err := v.marshal(req.source)

				if err != nil {
					req.result <- err
					continue
				}

				req.result <- v.value(value)
			}
		}
	}
}

func New(ctx context.Context) (*VM, error) {
	vm := &VM{
		requests:  make(chan Request),
		callbacks: make(map[string]*Callback),
	}

	vmReady := make(chan bool)
	go vm.poll(ctx, vmReady)
	<-vmReady

	return vm, nil
}
