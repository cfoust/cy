package janet

/*
#cgo CFLAGS: -std=c99
#cgo LDFLAGS: -lm -ldl

#include <janet.h>
#include <api.h>
*/
import "C"

import (
	"context"
	_ "embed"
	"errors"
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
	env := C.go_janet_core_env()
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

func (v *VM) handleCodeResult(call Call, out *Value) error {
	result := out.janet
	resultType := C.janet_type(result)

	if resultType == C.JANET_STRING {
		var message string
		err := v.unmarshal(result, &message)
		if err != nil {
			return err
		}

		return errors.New(message)
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
	} else {
		out.unroot()
	}

	return nil
}

// Run a string containing Janet code and return any error that occurs.
func (v *VM) runCode(params Params, call Call) {
	sourcePtr := C.CString(call.SourcePath)

	var env *C.JanetTable = C.go_janet_core_env()

	if v.env != nil {
		env = v.env.table
	}

	if len(call.Code) == 0 {
		params.Error(fmt.Errorf("empty code"))
		return
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

	fiber, err := v.createFiber(
		C.janet_unwrap_function(v.evaluate),
		args,
	)
	if err != nil {
		params.Error(err)
		return
	}
	subParams := params.Pipe()

	go func() {
		v.runFiber(subParams, fiber, nil)

		select {
		case <-subParams.Context.Done():
			params.Error(subParams.Context.Err())
			return
		case result := <-subParams.Result:
			if result.Error != nil {
				params.Error(result.Error)
				return
			}
			if result.Yield != nil {
				params.Yield(result.Yield)
				return
			}

			err := v.handleCodeResult(
				call,
				result.Out,
			)
			if err != nil {
				params.Error(err)
				return
			}

			params.Ok()
		}
	}()
}

// runFunction creates a fiber for the given function and arguments and runs
// it.
func (v *VM) runFunction(
	params Params,
	fun *C.JanetFunction,
	args []interface{},
) {
	cArgs := make([]C.Janet, 0)
	for _, arg := range args {
		value, err := v.marshal(arg)
		if err != nil {
			params.Error(err)
			return
		}
		cArgs = append(cArgs, value)
	}

	fiber, err := v.createFiber(
		fun,
		cArgs,
	)
	if err != nil {
		params.Error(err)
		return
	}

	go v.runFiber(params, fiber, nil)
}

// ExecuteCall executes a Call, which is the lowest-level interface for running
// Janet code.
func (v *VM) ExecuteCall(
	ctx context.Context,
	user interface{},
	call Call,
) (*Result, error) {
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
	return req.WaitResult()
}

func (v *VM) ExecuteFunction(
	ctx context.Context,
	user interface{},
	funcName string,
	params ...interface{},
) error {
	out, err := v.ExecuteCall(
		ctx,
		nil,
		CallString(fmt.Sprintf(
			`(yield %s)`,
			funcName,
		)),
	)
	if err != nil {
		return err
	}

	var fun *Function
	err = out.Yield.Unmarshal(&fun)
	if err != nil {
		return err
	}

	return fun.CallContext(ctx, user, params...)
}

func (v *VM) Execute(ctx context.Context, code string) error {
	_, err := v.ExecuteCall(ctx, nil, CallString(code))
	return err
}

// ExecuteFile executes a file containing Janet code.
func (v *VM) ExecuteFile(ctx context.Context, path string) error {
	bytes, err := readFile(path)
	if err != nil {
		return err
	}

	call := CallBytes(bytes)
	call.SourcePath = path

	_, err = v.ExecuteCall(ctx, nil, call)
	return err
}
