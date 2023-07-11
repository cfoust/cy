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
	"runtime"

	"github.com/sasha-s/go-deadlock"
)

//go:embed go-boot.janet
var GO_BOOT_FILE []byte

type Request interface{}

type RPC[A any, B any] struct {
	Params A
	Result chan B
}

func makeRPC[A any, B any](params A) RPC[A, B] {
	return RPC[A, B]{
		Params: params,
		Result: make(chan B),
	}
}

type VM struct {
	deadlock.RWMutex
	callbacks map[string]interface{}
	evaluate  C.Janet

	requests chan Request

	context interface{}
	env     *Table
}

func initJanet() {
	C.janet_init()
}

func deInitJanet() {
	C.janet_deinit()
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
	C.apply_env(env)
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
			case CallRequest:
				params := req.Params
				v.context = params.Context
				req.Result <- v.runCode(params)
				v.context = nil
			case UnlockRequest:
				req.Params.unroot()
			case FunctionRequest:
				params := req.Params
				v.context = params.Context
				req.Result <- v.runFunction(
					params.Function.function,
					params.Args,
				)
				v.context = nil
			}
		}
	}
}

func New(ctx context.Context) (*VM, error) {
	if globalVM != nil {
		return nil, fmt.Errorf("vm already initialized")
	}

	vm := VM{
		requests:  make(chan Request),
		callbacks: make(map[string]interface{}),
	}

	vmReady := make(chan bool)
	go vm.poll(ctx, vmReady)
	<-vmReady

	globalVM = &vm
	return &vm, nil
}
