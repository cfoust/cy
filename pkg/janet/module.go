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
	"sync"
	"sync/atomic"

	"github.com/sasha-s/go-deadlock"
)

//go:embed go-boot.janet
var GO_BOOT_FILE []byte

// We need some way of C calls to ExecGo back to their respective Janet
// instances.
var (
	nextId atomic.Int32
	vms    = sync.Map{}
)

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
	id int32

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

	// Store the VM's ID in the environment so calls to ExecGo can be
	// directed appropriately
	C.janet_table_put(
		env,
		C.janet_wrap_keyword(C.janet_ckeyword(VM_ID)),
		C.janet_wrap_integer(C.int(v.id)),
	)

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

func (v *VM) Free() {
	vms.Delete(v.id)
}

func New(ctx context.Context) (*VM, error) {
	vm := &VM{
		id:        nextId.Add(1),
		requests:  make(chan Request),
		callbacks: make(map[string]interface{}),
	}

	vmReady := make(chan bool)
	go vm.poll(ctx, vmReady)
	<-vmReady

	vms.Store(vm.id, vm)

	return vm, nil
}
