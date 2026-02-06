package janet

/*
#cgo CFLAGS: -std=c99
#cgo LDFLAGS: -lm -ldl

#define _POSIX_C_SOURCE 200809L
#include <janet.h>
#include <api.h>
#include <stdio.h>
*/
import "C"

import (
	"context"
	_ "embed"
	"errors"
	"fmt"
	"os"
	"unsafe"
)

type CallOptions struct {
	// Whether to allow the code to mutate the current environment.
	UpdateEnv bool
	// Dyns is a mapping of dynamic bindings that will be set while the code
	// executes
	Dyns map[Keyword]interface{}
}

const (
	FileFlagWrite        = int32(C.JANET_FILE_WRITE)
	FileFlagRead         = int32(C.JANET_FILE_READ)
	FileFlagAppend       = int32(C.JANET_FILE_APPEND)
	FileFlagUpdate       = int32(C.JANET_FILE_UPDATE)
	FileFlagNotCloseable = int32(C.JANET_FILE_NOT_CLOSEABLE)
	FileFlagBinary       = int32(C.JANET_FILE_BINARY)
)

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

	// Marshal dyns into a Janet table if provided and pass as 4th argument
	if len(params.Dyns) > 0 {
		dynsTable := C.janet_table(C.int(len(params.Dyns)))
		for name, value := range params.Dyns {
			marshaled, err := v.marshal(value)
			if err != nil {
				params.Error(
					fmt.Errorf("failed to marshal dyn %s: %w", name, err),
				)
				return
			}
			key := wrapKeyword(string(name))
			C.janet_table_put(dynsTable, key, marshaled)
		}
		args = append(args, C.janet_wrap_table(dynsTable))
	}

	fiber, err := v.createFiber(
		C.janet_unwrap_function(v.evaluate),
		args,
		params.Dyns,
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
		params.Dyns,
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
	result := make(chan Result, 1)
	req := callRequest{
		Params: Params{
			Context: ctx,
			User:    user,
			Result:  result,
			Dyns:    call.Options.Dyns,
		},
		Call: call,
	}
	select {
	case v.requests <- req:
	case <-ctx.Done():
		return nil, ctx.Err()
	}
	return req.WaitResult()
}

type wrapFileRequest struct {
	ctx    context.Context
	file   *os.File
	flags  int32
	result chan interface{}
}

// wrapFile is the internal implementation that runs on the VM thread.
func (v *VM) wrapFile(file *os.File, flags int32) (*Value, error) {
	mode := "r"
	if flags&FileFlagWrite != 0 && flags&FileFlagRead != 0 {
		mode = "r+"
	} else if flags&FileFlagWrite != 0 {
		mode = "w"
	}

	cMode := C.CString(mode)
	defer C.free(unsafe.Pointer(cMode))

	handle := C.fdopen(C.int(file.Fd()), cMode)
	if handle == nil {
		return nil, fmt.Errorf("fdopen failed")
	}

	janetFile := C.janet_makefile(
		handle,
		C.int(flags|FileFlagNotCloseable),
	)

	return v.value(janetFile), nil
}

// WrapFile converts an *os.File into a Janet file value that can be used as a
// dynamic stream (eg :in, :out, :err). The underlying file descriptor is not
// closed by Janet; callers are responsible for closing it.
func (v *VM) WrapFile(
	ctx context.Context,
	file *os.File,
	flags int32,
) (*Value, error) {
	if file == nil {
		return nil, fmt.Errorf("file is nil")
	}

	result := make(chan interface{})
	v.requests <- wrapFileRequest{
		ctx:    ctx,
		file:   file,
		flags:  flags,
		result: result,
	}

	select {
	case <-ctx.Done():
		return nil, ctx.Err()
	case res := <-result:
		switch res := res.(type) {
		case *Value:
			return res, nil
		case error:
			return nil, res
		}

		return nil, fmt.Errorf("never got result of WrapFile")
	}
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

	return fun.Call(ctx, user, Params{}, params...)
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
