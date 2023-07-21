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
	"unsafe"
)

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
	Context    interface{}
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
	Params
	Call
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

func (v *VM) handleCodeResult(params Params, call Call) error {
	var out *Value
	select {
	case <-params.Context.Done():
		return params.Context.Err()
	case result := <-params.Result:
		if result.Error != nil {
			return result.Error
		}
		out = result.Out
	}

	result := out.janet

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

// Run a string containing Janet code and return any error that occurs.
// TODO(cfoust): 07/20/23 send error to errc with timeout
func (v *VM) runCode(params Params, call Call) {
	sourcePtr := C.CString(call.SourcePath)

	var env *C.JanetTable = C.janet_core_env(nil)

	if v.env != nil {
		env = v.env.table
	}

	args := []C.Janet{
		C.janet_wrap_string(
			C.janet_string(
				(*C.uchar)(unsafe.Pointer(&(call.Code)[0])),
				C.int(len(call.Code)),
			),
		),
		C.janet_wrap_table(env),
		C.janet_wrap_string(
			C.janet_cstring(sourcePtr),
		),
	}

	fiber := v.createFiber(
		C.janet_unwrap_function(v.evaluate),
		args,
	)
	subParams := params.Pipe()

	go func() {
		v.runFiber(subParams, fiber, nil)
		err := v.handleCodeResult(subParams, call)
		if err != nil {
			params.Result <- Result{Error: err}
			return
		}
	}()
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

	argPtr := unsafe.Pointer(nil)
	if len(args) > 0 {
		argPtr = unsafe.Pointer(&cArgs[0])
	}

	var fiber *C.JanetFiber = nil
	var result C.Janet
	signal := C.janet_pcall(
		fun,
		C.int(len(args)),
		(*C.Janet)(argPtr),
		&result,
		&fiber,
	)

	switch signal {
	case C.JANET_SIGNAL_OK:
		return nil
	case C.JANET_SIGNAL_ERROR:
		return fmt.Errorf("error while running Janet function")
	default:
		return nil
	}
}

func (v *VM) ExecuteCall(ctx context.Context, user interface{}, call Call) error {
	result := make(chan Result)
	req := CallRequest{
		Params: Params{
			Context: ctx,
			User:    user,
			Result:  result,
		},
		Call: call,
	}
	v.requests <- req

	select {
	case result := <-req.Result:
		if result.Out != nil {
			result.Out.Free()
		}
		return result.Error
	case <-ctx.Done():
		return ctx.Err()
	}
}

func (v *VM) Execute(ctx context.Context, code string) error {
	return v.ExecuteCall(ctx, nil, CallString(code))
}

func (v *VM) ExecuteFile(ctx context.Context, path string) error {
	bytes, err := readFile(path)
	if err != nil {
		return err
	}

	call := CallBytes(bytes)
	call.SourcePath = path

	return v.ExecuteCall(ctx, nil, call)
}
