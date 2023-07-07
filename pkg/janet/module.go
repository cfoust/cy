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
	"io"
	"os"
	"reflect"
	"runtime"
	"unsafe"

	"github.com/sasha-s/go-deadlock"
)

//go:embed go-boot.janet
var GO_BOOT_FILE []byte

type Result struct {
	Error error
}

type CallOptions struct {
	// Whether to allow the code to mutate the current environment.
	UpdateEnv bool
}

var DEFAULT_CALL_OPTIONS = CallOptions{
	UpdateEnv: true,
}

type Call struct {
	Code       []byte
	SourcePath string
	Options    CallOptions
}

func CallBytes(data []byte) Call {
	return Call{
		Code:    data,
		Options: DEFAULT_CALL_OPTIONS,
	}
}

func CallString(code string) Call {
	return Call{
		Code:    []byte(code),
		Options: DEFAULT_CALL_OPTIONS,
	}
}

type CallRequest struct {
	Call   Call
	Result chan Result
}

// A request for a Janet value to be marked as freeable

type VM struct {
	deadlock.RWMutex
	callbacks map[string]interface{}
	evaluate  C.Janet

	execs   chan CallRequest
	unlocks chan *Value

	env *Environment
}

var ERROR_FREED = fmt.Errorf("cannot use freed value")

type Value struct {
	deadlock.RWMutex

	vm       *VM
	janet    C.Janet
	wasFreed bool
}

func (v *Value) IsFree() bool {
	v.RLock()
	defer v.RUnlock()
	return v.wasFreed
}

// Free, but synchronously.
func (v *Value) free() {
	v.Lock()
	defer v.Unlock()
	if v.wasFreed {
		return
	}
	v.wasFreed = true
	C.janet_gcunroot(v.janet)
}

func (v *Value) Free() {
	v.Lock()
	defer v.Unlock()
	if v.wasFreed {
		return
	}
	v.wasFreed = true

	v.vm.unlocks <- v
}

type Environment struct {
	value *Value
	table *C.JanetTable
}

// Run code without using our evaluation function. This can panic.
func (v *VM) runCodeUnsafe(code []byte, source string) {
	env := C.janet_core_env(nil)
	sourcePtr := C.CString(source)
	C.janet_dobytes(
		env,
		(*C.uchar)(unsafe.Pointer(&(code)[0])),
		C.int(len(code)),
		sourcePtr,
		nil,
	)
	C.free(unsafe.Pointer(sourcePtr))
}

func (v *VM) packValue(janet C.Janet) *Value {
	C.janet_gcroot(janet)
	return &Value{
		vm:    v,
		janet: janet,
	}
}

// Run code with our custom evaluator.
func (v *VM) runCode(call Call) error {
	sourcePtr := C.CString(call.SourcePath)

	var env *C.JanetTable = C.janet_core_env(nil)

	if v.env != nil {
		env = v.env.table
	}

	result := C.evaluate(
		v.evaluate,
		env,
		(*C.uchar)(unsafe.Pointer(&(call.Code)[0])),
		C.int(len(call.Code)),
		sourcePtr,
	)
	C.free(unsafe.Pointer(sourcePtr))

	resultType := C.janet_type(result)

	if resultType == C.JANET_STRING {
		var message string
		err := unmarshal(result, &message)
		if err != nil {
			return err
		}

		return fmt.Errorf(message)
	}

	if resultType != C.JANET_TABLE {
		return fmt.Errorf("evaluate returned unexpected type")
	}

	if call.Options.UpdateEnv {
		if v.env != nil {
			v.env.value.free()
		}

		v.env = &Environment{
			value: v.packValue(result),
			table: C.janet_unwrap_table(result),
		}
	}

	return nil
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

func readFile(filename string) ([]byte, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}

	defer file.Close()

	buffer, err := io.ReadAll(file)
	if err != nil {
		return nil, err
	}

	return buffer, nil
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

func (v *VM) ExecuteCall(call Call) error {
	req := CallRequest{
		Call:   call,
		Result: make(chan Result),
	}
	v.execs <- req
	return (<-req.Result).Error
}

func (v *VM) Execute(code string) error {
	return v.ExecuteCall(CallString(code))
}

func (v *VM) ExecuteFile(path string) error {
	bytes, err := readFile(path)
	if err != nil {
		return err
	}

	call := CallBytes(bytes)
	call.SourcePath = path

	return v.ExecuteCall(call)
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

	call := CallString(fmt.Sprintf(`
(def %s (go/make-callback "%s"))
`, name, name))
	call.Options.UpdateEnv = true
	v.ExecuteCall(call)

	return nil
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
