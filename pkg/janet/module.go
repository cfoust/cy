package janet

/*
#cgo CFLAGS: -std=c99
#cgo LDFLAGS: -lm -ldl

#include <janet.h>

void startVM() {
	// Initialize the virtual machine. Do this before any calls to Janet functions.
	janet_init();

	// Get the core janet environment. This contains all of the C functions in the core
	// as well as the code in src/boot/boot.janet.
	JanetTable *env = janet_core_env(NULL);

	// One of several ways to begin the Janet vm.
	janet_dostring(env, "(print `hello, world!`)", "main", NULL);
}

*/
import "C"

import (
	"context"
	"runtime"
)

type Result struct {
	Error error
}

type Call struct {
	// Execute some code as a string.
	Code string
	// Path to a .janet file to execute.
	Path   string
	Result chan<- Result
}

type VM struct {
	calls chan Call
}

func (v *VM) doString(code string) {
	env := C.janet_core_env(nil)

	// One of several ways to begin the Janet vm.
	C.janet_dostring(env, C.CString(code), C.CString("main"), nil)
}

// Wait for code calls and process them.
func (v *VM) poll(ctx context.Context) {
	// All Janet state is thread-local, so we explicitly want to execute
	// all Janet code in the same OS thread.
	runtime.LockOSThread()

	C.janet_init()
	defer C.janet_deinit()

	for {
		select {
		case <-ctx.Done():
			return
		case call := <-v.calls:
			v.doString(call.Code)
			call.Result <- Result{}
		}
	}
}

func (v *VM) Execute(code string) {
	result := make(chan Result)
	v.calls <- Call{
		Code:   code,
		Result: result,
	}
	<-result
}

func New(ctx context.Context) *VM {
	vm := VM{
		calls: make(chan Call),
	}
	go vm.poll(ctx)
	return &vm
}
