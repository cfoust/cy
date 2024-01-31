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

type callRequest struct {
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
	if fiber.fiber == nil {
		params.Error(fmt.Errorf("failed to create fiber"))
		return
	}
	subParams := params.Pipe()

	go func() {
		v.runFiber(subParams, fiber, nil)
		err := v.handleCodeResult(subParams, call)
		if err != nil {
			params.Error(err)
			return
		}

		params.Ok()
	}()
}

func (v *VM) runFunction(params Params, fun *C.JanetFunction, args []interface{}) {
	cArgs := make([]C.Janet, 0)
	for _, arg := range args {
		value, err := v.marshal(arg)
		if err != nil {
			params.Error(err)
			return
		}
		cArgs = append(cArgs, value)
	}

	fiber := v.createFiber(
		fun,
		cArgs,
	)
	if fiber.fiber == nil {
		params.Error(fmt.Errorf("failed to create fiber"))
		return
	}

	go v.runFiber(params, fiber, nil)
}

func (v *VM) ExecuteCall(ctx context.Context, user interface{}, call Call) error {
	result := make(chan Result)
	req := callRequest{
		Params: Params{
			Context: ctx,
			User:    user,
			Result:  result,
		},
		Call: call,
	}
	v.requests <- req
	return req.Wait()
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
