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
	"fmt"
	"unsafe"
)

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

type CallRequest RPC[Call, error]

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
		err := v.unmarshal(result, &message)
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
			Value: v.value(result),
			table: C.janet_unwrap_table(result),
		}
	}

	return nil
}

func (v *VM) runFunction(fun *C.JanetFunction, args []interface{}) error {
	cArgs := make([]C.Janet, 0)
	for _, arg := range args {
		value, err := v.marshal(arg)
		if err != nil {
			return err
		}
		cArgs = append(cArgs, value)
	}

	var fiber *C.JanetFiber = nil
	var result C.Janet
	C.janet_pcall(
		fun,
		C.int(len(args)),
		(*C.Janet)(unsafe.Pointer(&cArgs[0])),
		&result,
		&fiber,
	)

	return nil
}

func (v *VM) ExecuteCall(call Call) error {
	req := CallRequest{
		Params: call,
		Result: make(chan error),
	}
	v.requests <- req
	return <-req.Result
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
