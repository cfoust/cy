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

type CallRequest RPC[Call, error]

type FiberParams struct {
	Context interface{}
	// The fiber to run
	Fiber Fiber
	// The value with which to resume
	Value *Value
}

type FiberRequest RPC[FiberParams, error]

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

func (v *VM) runFiber(fiber *C.JanetFiber, value C.Janet, errc chan error) {
}

func (v *VM) handleYield(fiber *C.JanetFiber, value C.Janet, errc chan error) {
	if C.janet_checktype(value, C.JANET_ARRAY) == 0 {
		errc <- fmt.Errorf("(yield) called with non-array value")
		return
	}

	// We don't want any of the arguments to get garbage collected
	// before we're done executing
	C.janet_gcroot(value)

	args := make([]C.Janet, 0)
	for i := 0; i < int(C.janet_length(value)); i++ {
		args = append(
			args,
			C.janet_get(
				value,
				C.janet_wrap_integer(C.int(i)),
			),
		)
	}

	// go run the callback in a new goroutine
	go func() {
		defer C.janet_gcunroot(value)

		// TODO(cfoust): 07/20/23 ctx
		result, err := v.executeCallback(args)

		if err != nil {
			errc <- err
			return
		}

		// send a new event
		v.requests <- FiberRequest{
			Params: FiberParams{
				Context: v.context,
				Fiber: Fiber{
					Value: v.value(
						C.janet_wrap_fiber(fiber),
					),
					fiber: fiber,
				},
				Value: v.value(result),
			},
			Result: errc,
		}
	}()

	return
}

// Run a string containing Janet code and return any error that occurs.
// TODO(cfoust): 07/20/23 send error to errc with timeout
func (v *VM) runCode(call Call, errc chan error) {
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

	var fiber *C.JanetFiber = nil
	var result C.Janet
	signal := C.janet_pcall(
		C.janet_unwrap_function(v.evaluate),
		C.int(len(args)),
		(*C.Janet)(unsafe.Pointer(&args[0])),
		&result,
		&fiber,
	)
	C.free(unsafe.Pointer(sourcePtr))

	if signal == C.JANET_SIGNAL_YIELD {
		v.handleYield(fiber, result, errc)
		return
	}

	if signal != C.JANET_SIGNAL_OK {
		errc <- fmt.Errorf("failed to evaluate Janet code")
		return
	}

	resultType := C.janet_type(result)

	if resultType == C.JANET_STRING {
		var message string
		err := v.unmarshal(result, &message)
		if err != nil {
			errc <- err
			return
		}

		errc <- fmt.Errorf(message)
		return
	}

	if resultType != C.JANET_TABLE {
		errc <- fmt.Errorf("evaluate returned unexpected type")
		return
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

	errc <- nil
	return
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
