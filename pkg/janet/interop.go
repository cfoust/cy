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
	"strings"
	"unsafe"
)

type namable interface {
	getType() reflect.Type
	set(value reflect.Value)
}

type Named[T any] struct {
	value T
}

func (n *Named[T]) Values(defaults T) T {
	return n.value
}

func (n *Named[T]) getType() reflect.Type {
	return reflect.TypeOf(n.value)
}

func (n *Named[T]) set(value reflect.Value) {
	reflect.ValueOf(n.value).Set(value)
}

var _ namable = (*Named[int])(nil)

func getNamedParams(named namable) (params []string) {
	type_ := named.getType()

	for i := 0; i < type_.NumField(); i++ {
		params = append(
			params,
			getFieldName(type_.Field(i)),
		)
	}

	return
}

func wrapError(message string) C.Janet {
	return C.wrap_result_error(C.CString(message))
}

func isErrorType(type_ reflect.Type) bool {
	value := reflect.New(type_)
	_, ok := value.Interface().(*error)
	return ok
}

func handleReturn(v *VM, value reflect.Value) (C.Janet, error) {
	isPointer := value.Kind() == reflect.Pointer
	if isPointer {
		if value.IsNil() {
			return C.janet_wrap_nil(), nil
		}

		value = value.Elem()
	}

	return v.marshal(value.Interface())
}

// A request to receive the result of a callback.
type ResolveRequest struct {
	Params
	Fiber Fiber
	// The type signature of the callback
	Type reflect.Type
	// The results of the function call
	Out []reflect.Value
}

type PartialCallback struct {
	Type   reflect.Type
	invoke func() []reflect.Value
}

func (p *PartialCallback) Call() []reflect.Value {
	return p.invoke()
}

// Process Janet arguments and return a function that invokes the callback.
func (v *VM) setupCallback(params Params, args []C.Janet) (partial *PartialCallback, err error) {
	if len(args) == 0 {
		err = fmt.Errorf("you must provide at least one argument")
		return
	}

	target := args[0]
	var name string
	err = v.unmarshal(target, &name)
	if err != nil {
		return
	}

	v.RLock()
	callback, ok := v.callbacks[name]
	v.RUnlock()
	if !ok {
		err = fmt.Errorf("callback not found: %s", name)
		return
	}

	args = args[1:]

	callbackType := reflect.TypeOf(callback)
	callbackArgs := make([]reflect.Value, 0)

	argIndex := 0

	for i := 0; i < callbackType.NumIn(); i++ {
		argType := callbackType.In(i)
		argValue := reflect.New(argType)

		// Context allows for passing arbitrary vm-wide state to certain callbacks
		if isInterface(argType) {
			if _, ok := argValue.Interface().(*context.Context); ok {
				callbackArgs = append(callbackArgs, reflect.ValueOf(params.Context))
				continue
			}

			context := params.User
			if context == nil {
				callbackArgs = append(callbackArgs, reflect.New(argType).Elem())
			} else {
				callbackArgs = append(callbackArgs, reflect.ValueOf(context))
			}
			continue
		}

		if argIndex >= len(args) {
			err = fmt.Errorf("%s requires at least %d arguments", name, callbackType.NumIn())
			return
		}

		arg := args[argIndex]
		argIndex++

		isPointer := argType.Kind() == reflect.Pointer

		if isJanetFunction(argType) {
			isPointer = false
		}

		if isPointer {
			argValue = reflect.New(argType.Elem())
		}

		if isPointer && C.janet_checktype(arg, C.JANET_NIL) == 1 {
			argValue = reflect.NewAt(argType.Elem(), unsafe.Pointer(nil))
		} else {
			err = v.unmarshal(arg, argValue.Interface())
			if err != nil {
				err = fmt.Errorf("error processing argument %d: %s", argIndex, err.Error())
				return
			}

			if !isPointer {
				argValue = argValue.Elem()
			}
		}

		callbackArgs = append(callbackArgs, argValue)
	}

	partial = &PartialCallback{
		Type: callbackType,
		invoke: func() []reflect.Value {
			return reflect.ValueOf(callback).Call(callbackArgs)
		},
	}

	return
}

func (v *VM) resolveCallback(type_ reflect.Type, out []reflect.Value) (result C.Janet, resultErr error) {
	result = C.janet_wrap_nil()
	numResults := type_.NumOut()
	if numResults == 0 {
		return
	}

	if numResults == 1 {
		lastResult := out[0]

		if isErrorType(type_.Out(0)) {
			if err, ok := lastResult.Interface().(error); ok {
				resultErr = err
			}
			return
		}

		value, err := handleReturn(v, out[0])
		if err != nil {
			resultErr = fmt.Errorf("failed to marshal return value: %s", err.Error())
			return
		}

		result = value
		return
	}

	// numResults must be 2
	value, err := handleReturn(v, out[0])
	if err != nil {
		resultErr = fmt.Errorf("failed to marshal return value: %s", err.Error())
		return
	}

	result = value
	if err, ok := out[1].Interface().(error); ok {
		resultErr = err
	}

	return
}

func isInterface(type_ reflect.Type) bool {
	return type_.Kind() == reflect.Interface
}

func getNamable(type_ reflect.Type) namable {
	if type_.Kind() != reflect.Pointer {
		return nil
	}

	if named, ok := reflect.New(type_.Elem()).Interface().(namable); ok {
		return named
	}

	return nil
}

func isNamable(type_ reflect.Type) bool {
	return getNamable(type_) != nil
}

func isParamType(type_ reflect.Type) bool {
	if type_.Kind() == reflect.Pointer && isValidType(type_.Elem()) {
		return true
	}

	return isValidType(type_)
}

func (v *VM) Callback(name string, callback interface{}) error {
	type_ := reflect.TypeOf(callback)
	if type_.Kind() != reflect.Func {
		return fmt.Errorf("callback must be a function")
	}

	var named namable
	numNormal := 0

	numArgs := type_.NumIn()
	for i := 0; i < numArgs; i++ {
		argType := type_.In(i)

		named = getNamable(argType)
		if named != nil {
			namedType := named.getType()
			if namedType.Kind() != reflect.Struct {
				return fmt.Errorf("Named must have a struct type")
			}

			if !isValidType(namedType) {
				return fmt.Errorf("Named had field(s) with invalid types")
			}

			if i != numArgs-1 {
				return fmt.Errorf("Named must be the last argument")
			}

			numNormal = i
			break
		}

		if !isParamType(argType) && !isInterface(argType) {
			return fmt.Errorf(
				"arg %d's type %s (%s) not supported",
				i,
				argType.String(),
				argType.Kind().String(),
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
		if !isParamType(first) && !isErrorType(first) {
			return fmt.Errorf("first callback return type must be valid type or error")
		}
	}

	if numResults == 2 {
		if !isParamType(type_.Out(0)) {
			return fmt.Errorf("first callback return type must be valid type")
		}

		if !isErrorType(type_.Out(1)) {
			return fmt.Errorf("second callback return type must be error")
		}
	}

	v.Lock()
	v.callbacks[name] = callback
	v.Unlock()

	code := fmt.Sprintf(
		`[& args] (go/callback "%s" ;args)`,
		name,
	)

	if named != nil {
		args := make([]string, 0)
		for i := 0; i < numNormal; i++ {
			args = append(args, fmt.Sprintf("arg%d ", i))
		}

		params := getNamedParams(named)

		argStr := strings.Join(args, " ")
		paramStr := strings.Join(params, " ")
		code = fmt.Sprintf(
			`[%s &named %s] (go/callback "%s" %s %s)`,
			argStr,
			paramStr,
			name,
			argStr,
			paramStr,
		)
	}

	call := CallString(fmt.Sprintf(`
(def %s (fn %s))
`, name, code))
	call.Options.UpdateEnv = true
	err := v.ExecuteCall(context.Background(), nil, call)
	if err != nil {
		return err
	}

	return nil
}
