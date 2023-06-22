package janet

/*
#cgo CFLAGS: -std=c99
#cgo LDFLAGS: -lm -ldl

#include <janet.h>
#include <src.h>
*/
import "C"

import (
	"context"
	"fmt"
	"reflect"
	"runtime"
)

//export ExecGo
func ExecGo(callback *C.char) int64 {
	fmt.Printf(C.GoString(callback))
	return 0
}

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
	C.apply_env(env)
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

func (v *VM) Execute(code string) error {
	result := make(chan Result)
	v.calls <- Call{
		Code:   code,
		Result: result,
	}
	return (<-result).Error
}

func isValidType(type_ reflect.Type) bool {
	switch type_.Kind() {
	case reflect.Int, reflect.Float32, reflect.String:
		return true
	default:
		return false
	}
}

func (v *VM) RegisterCallback(name string, callback interface{}) error {
	type_ := reflect.TypeOf(callback)
	if type_.Kind() != reflect.Func {
		return fmt.Errorf("callback must be a function")
	}

	for i := 0; i < type_.NumIn(); i++ {
		argType := type_.In(i)

		if !isValidType(argType) {
			return fmt.Errorf(
				"arg %d's type %s not supported",
				i,
				argType.String(),
			)
		}
	}

	if type_.NumOut() > 1 {
		return fmt.Errorf("callback has too many return values")
	}

	if type_.NumOut() == 1 && !isValidType(type_.Out(0)) {
		return fmt.Errorf("callback has invalid return type")
	}

	return nil
}

func New(ctx context.Context) *VM {
	vm := VM{
		calls: make(chan Call),
	}
	go vm.poll(ctx)
	return &vm
}
