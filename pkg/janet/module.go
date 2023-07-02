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
	"runtime"

	"github.com/sasha-s/go-deadlock"
)

//go:embed go-boot.janet
var GO_BOOT_FILE []byte

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
	deadlock.RWMutex
	calls     chan Call
	callbacks map[string]interface{}
}

func (v *VM) doString(code string) {
	env := C.janet_core_env(nil)
	C.apply_env(env)
	C.janet_dostring(env, C.CString(code), C.CString("."), nil)
}

var globalVM *VM = nil

func wrapError(message string) C.Janet {
	return C.wrap_result_error(C.CString(message))
}

//export ExecGo
func ExecGo(argc int, argv *C.Janet) C.Janet {
	if globalVM == nil {
		return wrapError("vm not initialized")
	}

	args := make([]C.Janet, 0)
	for i := 0; i < argc; i++ {
		args = append(args, C.access_argv(argv, C.int(i)))
	}

	result, err := globalVM.executeCallback(args)
	if err != nil {
		return wrapError(err.Error())
	}

	return C.wrap_result_value(result)
}

func initJanet() {
	C.janet_init()
}

func deInitJanet() {
	C.janet_deinit()
}

// Wait for code calls and process them.
func (v *VM) poll(ctx context.Context) {
	// All Janet state is thread-local, so we explicitly want to execute
	// all Janet code in the same OS thread.
	runtime.LockOSThread()

	initJanet()
	defer deInitJanet()

	env := C.janet_core_env(nil)
	C.apply_env(env)

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

func isErrorType(type_ reflect.Type) bool {
	value := reflect.New(type_)
	_, ok := value.Interface().(*error)
	return ok
}

func (v *VM) executeCallback(args []C.Janet) (result C.Janet, resultErr error) {
	result = C.janet_wrap_nil()

	if len(args) == 0 {
		resultErr = fmt.Errorf("you must provide at least one argument")
		return
	}

	target := args[0]
	var name string
	err := unmarshal(target, &name)
	if err != nil {
		resultErr = err
		return
	}

	v.RLock()
	callback, ok := v.callbacks[name]
	v.RUnlock()
	if !ok {
		resultErr = fmt.Errorf("callback not found: %s", name)
		return
	}

	args = args[1:]

	callbackType := reflect.TypeOf(callback)
	callbackArgs := make([]reflect.Value, 0)

	for i := 0; i < callbackType.NumIn(); i++ {
		argType := callbackType.In(i)
		argValue := reflect.New(argType)

		if i >= len(args) {
			resultErr = fmt.Errorf("%s requires at least %d arguments", name, callbackType.NumIn())
			return
		}

		err := unmarshal(args[i], argValue.Interface())
		if err != nil {
			resultErr = fmt.Errorf("error processing argument %d: %s", i, err.Error())
			return
		}

		callbackArgs = append(callbackArgs, argValue.Elem())
	}

	results := reflect.ValueOf(callback).Call(callbackArgs)
	numResults := callbackType.NumOut()
	if numResults == 0 {
		return
	}

	if numResults == 1 {
		if isErrorType(callbackType.Out(0)) {
			if err, ok := results[0].Interface().(error); ok {
				resultErr = err
			}
			return
		}

		value, err := marshal(results[0].Interface())
		if err != nil {
			resultErr = fmt.Errorf("failed to marshal return value: %s", err.Error())
			return
		}

		result = value
		return
	}

	// numResults must be 2
	value, err := marshal(results[0].Interface())
	if err != nil {
		resultErr = fmt.Errorf("failed to marshal return value: %s", err.Error())
		return
	}

	result = value
	if err, ok := results[1].Interface().(error); ok {
		resultErr = err
	}

	return
}

func (v *VM) Callback(name string, callback interface{}) error {
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

	numResults := type_.NumOut()

	if numResults > 2 {
		return fmt.Errorf("callback has too many return values")
	}

	// The first return value can be an error or valid type
	if numResults == 1 {
		first := type_.Out(0)
		if !isValidType(first) && !isErrorType(first) {
			return fmt.Errorf("first callback return type must be valid type or error")
		}
	}

	if numResults == 2 {
		if !isValidType(type_.Out(0)) {
			return fmt.Errorf("first callback return type must be valid type")
		}

		if !isErrorType(type_.Out(1)) {
			return fmt.Errorf("second callback return type must be error")
		}
	}

	v.Lock()
	v.callbacks[name] = callback
	v.Unlock()

	return nil
}

func New(ctx context.Context) (*VM, error) {
	if globalVM != nil {
		return nil, fmt.Errorf("vm already initialized")
	}

	vm := VM{
		calls:     make(chan Call),
		callbacks: make(map[string]interface{}),
	}
	go vm.poll(ctx)
	globalVM = &vm
	return &vm, nil
}
