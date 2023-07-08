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

	env *Table
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
			v.env.unroot()
		}

		v.env = &Table{
			Value: v.packValue(result),
			table: C.janet_unwrap_table(result),
		}
	}

	return nil
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
