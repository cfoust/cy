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

	"github.com/iancoleman/strcase"
)

// Renamable provides a mapping from Go method names to Janet function names.
// This is useful for declaring Janet functions that are not valid Go, such as
// "boolean?".
type Renamable interface {
	Renames() map[string]string
}

// Docstrings provides docstrings for module methods. This is a mapping from
// the method's Go name to its Janet docstring.
type Docstrings interface {
	Docstrings() map[string]string
}

// This is a little weird, but it's a workaround to allow us to simplify the
// public API for setting up named parameters.
type nameable interface {
	getType() reflect.Type
	set(interface{})
}

type Named[T any] struct {
	value T
}

func (n *Named[T]) WithDefault(defaults T) T {
	updated := n.value

	type_ := n.getType()
	dstValue := reflect.ValueOf(&updated)
	defaultValue := reflect.ValueOf(defaults)

	for i := 0; i < type_.NumField(); i++ {
		defaultField := defaultValue.Field(i)
		dstField := dstValue.Elem().Field(i)

		switch defaultField.Type().Kind() {
		case reflect.Slice, reflect.Array:
			if defaultField.Len() > 0 && dstField.Len() == 0 {
				dstField.Set(defaultField)
			}
			continue
		}

		// If defaultField's value is the type's default value, use the
		// value from default
		if dstField.Interface() == reflect.New(dstField.Type()).Elem().Interface() {
			dstField.Set(defaultField)
		}
	}

	return updated
}

func (n *Named[T]) Values() T {
	return n.value
}

func (n *Named[T]) getType() reflect.Type {
	return reflect.TypeOf(n.value)
}

func (n *Named[T]) set(newValue interface{}) {
	if value, ok := newValue.(T); ok {
		n.value = value
	}
}

var _ nameable = (*Named[int])(nil)

func getNamedParams(named nameable) (params []string) {
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
	if value.Kind() == reflect.Pointer {
		if value.IsNil() {
			return C.janet_wrap_nil(), nil
		}

		if v, ok := value.Interface().(*Value); ok {
			return v.janet, nil
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

	callbackType := callback.function.Type()
	callbackArgs := make([]reflect.Value, 0)

	startIndex := 0

	if callback.source != nil {
		startIndex = 1
		callbackArgs = append(
			callbackArgs,
			reflect.ValueOf(callback.source),
		)
	}

	argIndex := 0

	for i := startIndex; i < callbackType.NumIn(); i++ {
		argType := callbackType.In(i)
		argValue := reflect.New(argType)

		// Handle all of the remaining named parameters
		named := getNamable(argType)
		if named != nil {
			type_ := named.getType()
			value := reflect.New(type_).Elem()

			for j := 0; j < type_.NumField(); j++ {
				field := type_.Field(j)
				fieldValue := value.Field(j)

				namedArg := args[argIndex+j]
				if C.janet_checktype(namedArg, C.JANET_NIL) == 1 {
					continue
				}

				err := v.unmarshal(
					namedArg,
					fieldValue.Addr().Interface(),
				)
				if err != nil {
					return nil, fmt.Errorf(
						"failed to unmarshal named param %s: %s",
						getFieldName(field),
						err.Error(),
					)
				}
			}

			named.set(value.Interface())
			callbackArgs = append(callbackArgs, reflect.ValueOf(named))
			break
		}

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

		isPointer := argType.Kind() == reflect.Pointer

		if isSpecial(argType) {
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
				err = fmt.Errorf(
					"%s: error processing argument %d: %s",
					name,
					argIndex,
					err.Error(),
				)
				return
			}

			if !isPointer {
				argValue = argValue.Elem()
			}
		}

		callbackArgs = append(callbackArgs, argValue)
		argIndex++
	}

	partial = &PartialCallback{
		Type: callbackType,
		invoke: func() []reflect.Value {
			return callback.function.Call(callbackArgs)
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

func getNamable(type_ reflect.Type) nameable {
	if type_.Kind() != reflect.Pointer {
		return nil
	}

	if named, ok := reflect.New(type_.Elem()).Interface().(nameable); ok {
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

type Callback struct {
	source   interface{}
	function reflect.Value
}

func validateFunction(in, out []reflect.Type) error {
	numArgs := len(in)
	for i := 0; i < numArgs; i++ {
		argType := in[i]

		named := getNamable(argType)
		if named != nil {
			namedType := named.getType()
			if namedType.Kind() != reflect.Struct {
				return fmt.Errorf("Named must have a struct type")
			}

			for j := 0; j < namedType.NumField(); j++ {
				field := namedType.Field(j)
				if !isParamType(field.Type) {
					return fmt.Errorf(
						"Named has invalid field %d (%s)",
						j,
						field.Name,
					)
				}
			}

			if i != numArgs-1 {
				return fmt.Errorf("Named must be the last argument")
			}
			break
		}

		if !isSpecial(argType) && !isParamType(argType) && !isInterface(argType) {
			return fmt.Errorf(
				"arg %d's type %s (%s) not supported",
				i,
				argType.String(),
				argType.Kind().String(),
			)
		}
	}

	numResults := len(out)

	if numResults > 2 {
		return fmt.Errorf("callback has too many return values")
	}

	// The first return value can be an error or valid type
	if numResults == 1 {
		first := out[0]
		if !isParamType(first) && !isErrorType(first) && !isInterface(first) {
			return fmt.Errorf("first callback return type must be valid type or error")
		}
	}

	if numResults == 2 {
		if !isParamType(out[0]) && !isInterface(out[0]) {
			return fmt.Errorf("first callback return type must be valid type")
		}

		if !isErrorType(out[1]) {
			return fmt.Errorf("second callback return type must be error")
		}
	}

	return nil
}

func getPrototype(name string, in, out []reflect.Type) (string, error) {
	var argCount int
	var argList []string
	var namedParams []string

	numArgs := len(in)
	for i := 0; i < numArgs; i++ {
		argType := in[i]
		named := getNamable(argType)

		if isInterface(argType) {
			continue
		}

		if named == nil {
			argList = append(argList, fmt.Sprintf("arg%d ", argCount))
			argCount++
			continue
		}

		if i != numArgs-1 {
			return "", fmt.Errorf("named parameter must be last")
		}

		namedParams = getNamedParams(named)
	}

	invocation := argList
	if len(namedParams) > 0 {
		argList = append(argList, "&named")
		argList = append(argList, namedParams...)
		invocation = append(invocation, namedParams...)
	}

	return fmt.Sprintf(
		`[%s] (go/callback "%s" %s)`,
		strings.Join(argList, " "),
		name,
		strings.Join(invocation, " "),
	), nil
}

func getFunctionTypes(f reflect.Value) (in, out []reflect.Type, err error) {
	type_ := f.Type()
	if type_.Kind() != reflect.Func {
		err = fmt.Errorf("callback must be a function")
		return
	}

	for i := 0; i < type_.NumIn(); i++ {
		in = append(in, type_.In(i))
	}

	for i := 0; i < type_.NumOut(); i++ {
		out = append(out, type_.Out(i))
	}

	return
}

func (v *VM) registerCallback(
	name string,
	docstring string,
	source interface{},
	callback reflect.Value,
) error {
	in, out, err := getFunctionTypes(callback)
	if err != nil {
		return err
	}

	// The first argument for struct methods is the receiver; we do not
	// need to validate it
	if source != nil {
		in = in[1:]
	}

	err = validateFunction(in, out)
	if err != nil {
		return fmt.Errorf("could not register %s: %s", name, err.Error())
	}

	v.Lock()
	v.callbacks[name] = &Callback{
		source:   source,
		function: callback,
	}
	v.Unlock()

	prototype, err := getPrototype(name, in, out)
	if err != nil {
		return err
	}

	docstring = strings.TrimSpace(docstring)

	format := "(defn %s %s %s)"

	// You can provide a custom method prototype by providing a docstring
	// that begins with "([method name]"
	// This stops Janet from auto-generating a prototype for your
	// docstring, which is desirable because there is no way to detect
	// argument names using reflection
	if strings.HasPrefix(docstring, "("+name) {
		format = "(def %s %s (fn %s))"
	}

	if len(docstring) > 0 {
		// Janet lets you enclose string literals enclosed by as many `
		// characters as you like; this is a lazy way of "escaping" the
		// docstring
		docstring = "`````" + docstring + "\n`````"
	} else {
		docstring = `""`
	}

	code := fmt.Sprintf(
		format,
		name,
		docstring,
		prototype,
	)
	call := CallString(code)
	call.Options.UpdateEnv = true
	err = v.ExecuteCall(context.Background(), nil, call)
	if err != nil {
		return err
	}

	return nil
}

func (v *VM) Callback(name string, docstring string, callback interface{}) error {
	return v.registerCallback(
		name,
		docstring,
		nil,
		reflect.ValueOf(callback),
	)
}

func (v *VM) Module(name string, module interface{}) error {
	type_ := reflect.TypeOf(module)

	if type_.Kind() == reflect.Pointer {
		type_ = type_.Elem()
	}

	if type_.Kind() != reflect.Struct {
		return fmt.Errorf("module must be a struct")
	}

	type_ = reflect.TypeOf(module)
	renamable, haveRenames := module.(Renamable)
	docs := make(map[string]string)

	if docstrings, ok := module.(Docstrings); ok {
		docs = docstrings.Docstrings()
	}
	if documented, ok := module.(Documented); ok {
		docs = parseDocstrings(documented.Documentation())
	}

	for i := 0; i < type_.NumMethod(); i++ {
		method := type_.Method(i)
		methodName := strcase.ToKebab(method.Name)

		if (haveRenames && methodName == "renames") || methodName == "docstrings" || methodName == "documentation" {
			continue
		}

		if haveRenames {
			if replaced, ok := renamable.Renames()[method.Name]; ok {
				methodName = replaced
			}
		}

		docstring, _ := docs[method.Name]

		err := v.registerCallback(
			fmt.Sprintf(
				"%s/%s",
				name,
				methodName,
			),
			docstring,
			module,
			method.Func,
		)
		if err != nil {
			return err
		}
	}

	return nil
}
