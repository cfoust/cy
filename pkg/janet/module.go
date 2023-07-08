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

type VM struct {
	deadlock.RWMutex
	callbacks map[string]interface{}
	evaluate  C.Janet

	execs   chan CallRequest
	unlocks chan *Value

	env *Table
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
		case req := <-v.execs:
			call := req.Call
			req.Result <- Result{
				Error: v.runCode(call),
			}
		case req := <-v.unlocks:
			C.janet_gcunroot(req.janet)
		}
	}
}

func New(ctx context.Context) (*VM, error) {
	if globalVM != nil {
		return nil, fmt.Errorf("vm already initialized")
	}

	vm := VM{
		execs:     make(chan CallRequest),
		unlocks:   make(chan *Value),
		callbacks: make(map[string]interface{}),
	}

	vmReady := make(chan bool)
	go vm.poll(ctx, vmReady)
	<-vmReady

	globalVM = &vm
	return &vm, nil
}
