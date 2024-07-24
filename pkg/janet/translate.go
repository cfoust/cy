package janet

/*
#cgo CFLAGS: -std=c99
#cgo LDFLAGS: -lm -ldl

#include <stdlib.h>
#include <janet.h>
#include <api.h>
*/
import "C"

import (
	"fmt"
	"reflect"
	"strings"
	"unsafe"

	"github.com/iancoleman/strcase"
)

// Marshalable lets you define a custom (pure) transformation to perform on
// this type prior to translating it into a Janet value. The MarshalJanet()
// implementation must have a _value_ receiver.
type Marshalable interface {
	MarshalJanet() interface{}
}

// Unmarshalable lets you define custom code for turning a Janet value into
// this type. The UnmarshalJanet() implementation must have a _pointer_
// receiver.
type Unmarshalable interface {
	UnmarshalJanet(value *Value) error
}

var MARSHALABLE = reflect.TypeOf((*Marshalable)(nil)).Elem()
var UNMARSHALABLE = reflect.TypeOf((*Unmarshalable)(nil)).Elem()

// checkInterfaces does a sanity check on the (Un)Marshalable types. There are
// some mistakes that can't be checked automatically at compile time. This is
// an issue with golang: to cast to an interface, the receivers of all of that
// interface's methods must be of the same kind (pointer or value) in order for
// the casting statement to return "ok".
//
// We specifically want Unmarshable to have a pointer receiver and Marshalable
// to have a value receiver, because that's how the serde library works.
func checkInterfaces(type_ reflect.Type, value reflect.Value) error {
	if _, ok := value.Interface().(Marshalable); ok {
		if !type_.Implements(UNMARSHALABLE) {
			return fmt.Errorf("implementation of Unmarshalable missing for %s", type_.String())
		}
	}

	if _, ok := value.Interface().(Unmarshalable); ok {
		if value.Kind() != reflect.Pointer {
			return fmt.Errorf("implementation of Unmarshalable for %s should have pointer receiver", type_.String())
		}

		if !type_.Implements(MARSHALABLE) {
			return fmt.Errorf("implementation of Marshalable missing for %s", type_.String())
		}
	}

	return nil
}

// Wrap a string as a Janet keyword.
func wrapKeyword(word string) C.Janet {
	str := C.CString(word)
	keyword := C.wrap_keyword(str)
	C.free(unsafe.Pointer(str))
	return keyword
}

func isSpecial(type_ reflect.Type) bool {
	if _, ok := reflect.New(type_).Elem().Interface().(*Function); ok {
		return true
	}

	if _, ok := reflect.New(type_).Elem().Interface().(*Value); ok {
		return true
	}

	return false
}

func isTuple(type_ reflect.Type) bool {
	if type_.Kind() != reflect.Struct {
		return false
	}

	if type_.NumField() == 0 {
		return false
	}

	first := type_.Field(0)
	return first.Name == "_" && first.Tag.Get("janet") == "tuple"
}

func getFieldName(field reflect.StructField) (name string) {
	if custom, ok := field.Tag.Lookup("janet"); ok {
		return custom
	}

	return strcase.ToKebab(field.Name)
}

func isValidType(type_ reflect.Type) bool {
	switch type_.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Float64, reflect.Bool, reflect.String:
		return true
	case reflect.Pointer:
		if _, ok := reflect.New(type_.Elem()).Interface().(*Value); ok {
			return true
		}

		return false
	case reflect.Struct:
		value := reflect.New(type_).Elem()
		for i := 0; i < type_.NumField(); i++ {
			if !value.Field(i).CanInterface() {
				continue
			}

			if !isValidType(type_.Field(i).Type) {
				return false
			}
		}
		return true
	case reflect.Array, reflect.Slice:
		return isValidType(type_.Elem())
	default:
		return false
	}
}

// IsValidType returns true if the given value can be translated to a Janet value.
func IsValidType(value interface{}) bool {
	return isValidType(reflect.TypeOf(value))
}

// marshal turns a Go value into a Janet value. This is a low-level interface.
func (v *VM) marshal(item interface{}) (result C.Janet, err error) {
	result = C.janet_wrap_nil()

	if item == nil {
		return
	}

	type_ := reflect.TypeOf(item)
	value := reflect.ValueOf(item)

	if m, ok := value.Interface().(Marshalable); ok {
		err = checkInterfaces(type_, value)
		if err != nil {
			return
		}

		return v.marshal(m.MarshalJanet())
	}

	if value.Kind() == reflect.Pointer {
		if v, ok := item.(*Value); ok {
			// TODO(cfoust): 02/01/24 this is spooky
			if v == nil {
				return
			}
			result = v.janet
			return
		}

		// TODO(cfoust): 06/23/23 maybe support this someday
		err = fmt.Errorf("cannot marshal pointer to value")
		return
	}

	switch type_.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32:
		result = C.janet_wrap_integer(C.int(value.Int()))
	case reflect.Float64:
		result = C.janet_wrap_number(C.double(value.Float()))
	case reflect.Bool:
		boolean := value.Bool()
		if boolean {
			result = C.janet_wrap_boolean(1)
		} else {
			result = C.janet_wrap_boolean(0)
		}
	case reflect.String:
		strPtr := C.CString(value.String())
		if _, ok := item.(Keyword); ok {
			result = C.wrap_keyword(strPtr)
		} else {
			result = C.janet_wrap_string(C.janet_cstring(strPtr))
		}
		defer C.free(unsafe.Pointer(strPtr))
	case reflect.Struct:
		if isTuple(type_) {
			elements := make([]C.Janet, 0)
			for i := 1; i < type_.NumField(); i++ {
				field := type_.Field(i)
				fieldValue := value.Field(i)
				value_, fieldErr := v.marshal(fieldValue.Interface())
				if fieldErr != nil {
					err = fmt.Errorf(
						"could not marshal value '%s': %s",
						field.Name,
						fieldErr.Error(),
					)
					return
				}
				elements = append(elements, value_)
			}

			argPtr := unsafe.Pointer(nil)
			if len(elements) > 0 {
				argPtr = unsafe.Pointer(&elements[0])
			}

			result = C.janet_wrap_tuple(
				C.janet_tuple_n(
					(*C.Janet)(argPtr),
					C.int(len(elements)),
				),
			)
			return
		}

		struct_ := C.janet_struct_begin(C.int(type_.NumField()))
		for i := 0; i < type_.NumField(); i++ {
			field := type_.Field(i)
			fieldValue := value.Field(i)

			if !fieldValue.CanInterface() {
				continue
			}

			key_ := wrapKeyword(getFieldName(field))

			value_, fieldErr := v.marshal(fieldValue.Interface())
			if fieldErr != nil {
				err = fmt.Errorf("could not marshal value '%s': %s", field.Name, fieldErr.Error())
				return
			}

			C.janet_struct_put(struct_, key_, value_)
		}
		result = C.janet_wrap_struct(C.janet_struct_end(struct_))
	case reflect.Array, reflect.Slice:
		numElements := 0
		if type_.Kind() == reflect.Array {
			numElements = type_.Len()
		} else {
			numElements = value.Len()
		}

		array := C.janet_array(C.int(numElements))
		for i := 0; i < numElements; i++ {
			value_, indexErr := v.marshal(value.Index(i).Interface())
			if indexErr != nil {
				err = indexErr
				return
			}

			C.janet_array_push(array, value_)
		}
		result = C.janet_wrap_array(array)
	default:
		err = fmt.Errorf("unimplemented type: %s", type_.String())
		return
	}

	return
}

var JANET_TYPE_TO_STRING map[C.JanetType]string = map[C.JanetType]string{
	C.JANET_NUMBER:    "number",
	C.JANET_NIL:       "nil",
	C.JANET_BOOLEAN:   "boolean",
	C.JANET_FIBER:     "fiber",
	C.JANET_STRING:    "string",
	C.JANET_SYMBOL:    "symbol",
	C.JANET_KEYWORD:   "keyword",
	C.JANET_ARRAY:     "array",
	C.JANET_TUPLE:     "tuple",
	C.JANET_TABLE:     "table",
	C.JANET_STRUCT:    "struct",
	C.JANET_BUFFER:    "buffer",
	C.JANET_FUNCTION:  "function",
	C.JANET_CFUNCTION: "cfunction",
	C.JANET_ABSTRACT:  "abstract",
	C.JANET_POINTER:   "pointer",
}

func janetTypeString(type_ C.JanetType) string {
	mapping, ok := JANET_TYPE_TO_STRING[type_]
	if !ok {
		return "unknown"
	}

	return mapping
}

func assertType(value C.Janet, expected ...C.JanetType) (err error) {
	expectedStrs := make([]string, 0)
	for _, type_ := range expected {
		if C.janet_checktype(value, type_) == 1 {
			return
		}
		expectedStrs = append(expectedStrs, janetTypeString(type_))
	}

	actual := C.janet_type(value)
	// TODO(cfoust): 07/08/23 this is wrong
	return fmt.Errorf(
		"expected %s, got %s",
		strings.Join(expectedStrs, "|"),
		janetTypeString(actual),
	)
}

func prettyPrint(value C.Janet) string {
	ptr := C._pretty_print(value)
	return strings.Clone(C.GoString(ptr))
}

func (v *VM) unmarshal(source C.Janet, dest interface{}) error {
	type_ := reflect.TypeOf(dest)
	value := reflect.ValueOf(dest)

	if value.Kind() != reflect.Pointer {
		return fmt.Errorf("cannot unmarshal into non-pointer value")
	}

	if u, ok := value.Interface().(Unmarshalable); ok {
		err := checkInterfaces(type_, value)
		if err != nil {
			return err
		}

		janetValue := v.value(source)
		janetValue.isSafe = true
		defer janetValue.unroot()
		return u.UnmarshalJanet(janetValue)
	}

	type_ = type_.Elem()
	value = value.Elem()

	switch type_.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32:
		if err := assertType(source, C.JANET_NUMBER); err != nil {
			return err
		}
		unwrapped := C.janet_unwrap_integer(source)
		value.SetInt(int64(unwrapped))
	case reflect.Float64:
		if err := assertType(source, C.JANET_NUMBER); err != nil {
			return err
		}
		unwrapped := C.janet_unwrap_number(source)
		value.SetFloat(float64(unwrapped))
	case reflect.Bool:
		if err := assertType(source, C.JANET_BOOLEAN); err != nil {
			return err
		}
		unwrapped := C.janet_unwrap_boolean(source)
		if unwrapped == 1 {
			value.SetBool(true)
		} else {
			value.SetBool(false)
		}
	case reflect.String:
		if keyword, ok := value.Interface().(Keyword); ok {
			if err := assertType(source, C.JANET_KEYWORD); err != nil {
				return err
			}

			strPtr := strings.Clone(C.GoString(C.cast_janet_string(C.janet_unwrap_keyword(source))))

			// if the keyword already contains a value, act as if
			// we're comparing against a constant
			keywordValue := string(keyword)
			if len(keywordValue) == 0 {
				value.SetString(strings.Clone(strPtr))
			} else if strPtr != keywordValue {
				return fmt.Errorf("keyword :%s does not match :%s", strPtr, keyword)
			}

			return nil
		}

		if err := assertType(source, C.JANET_STRING); err != nil {
			return err
		}
		strPtr := C.GoString(C.cast_janet_string(C.janet_unwrap_string(source)))
		value.SetString(strings.Clone(strPtr))
	case reflect.Pointer:
		// TODO(cfoust): 07/08/23 this is messy, fix it
		if _, ok := reflect.New(type_).Elem().Interface().(*Function); ok {
			if err := assertType(source, C.JANET_FUNCTION); err != nil {
				return err
			}

			function := &Function{
				Value:    v.value(source),
				function: C.janet_unwrap_function(source),
			}
			value.Set(reflect.ValueOf(function))
			return nil
		}

		if _, ok := reflect.New(type_).Elem().Interface().(*Value); ok {
			value.Set(reflect.ValueOf(v.value(source)))
			return nil
		}

		if C.janet_checktype(source, C.JANET_NIL) != 1 {
			ptr := reflect.New(type_.Elem())
			err := v.unmarshal(source, ptr.Interface())
			if err != nil {
				return err
			}
			value.Set(ptr)
		} else {
			value.Set(reflect.ValueOf(nil))
		}

		//return fmt.Errorf("unimplemented pointer type: %s (%s)", type_.String(), type_.Kind().String())
		return nil
	case reflect.Struct:
		if isTuple(type_) {
			if err := assertType(source, C.JANET_TUPLE); err != nil {
				return err
			}

			tuple := C.janet_unwrap_tuple(source)

			numElements := C.tuple_length(tuple)
			requiredElements := C.int(type_.NumField() - 1)
			if numElements != requiredElements {
				return fmt.Errorf(
					"tuple did not have enough elements (%d vs %d)",
					numElements,
					requiredElements,
				)
			}

			// Skip first field, it's "_"
			for i := 1; i < type_.NumField(); i++ {
				field := type_.Field(i)
				fieldValue := value.Field(i)

				if !fieldValue.Addr().CanInterface() {
					continue
				}

				err := v.unmarshal(
					C.access_argv(tuple, C.int(i-1)),
					fieldValue.Addr().Interface(),
				)
				if err != nil {
					return fmt.Errorf(
						"failed to unmarshal tuple field %s: %s",
						field.Name,
						err.Error(),
					)
				}
			}
			return nil
		}

		if err := assertType(source, C.JANET_STRUCT); err != nil {
			return err
		}

		struct_ := C.janet_unwrap_struct(source)
		for i := 0; i < type_.NumField(); i++ {
			field := type_.Field(i)
			fieldValue := value.Field(i)

			if !fieldValue.Addr().CanInterface() {
				continue
			}

			key_ := wrapKeyword(getFieldName(field))
			value_ := C.janet_struct_get(struct_, key_)
			err := v.unmarshal(value_, fieldValue.Addr().Interface())
			if err != nil {
				return fmt.Errorf("failed to unmarshal struct field %s: %s", field.Name, err.Error())
			}
		}
	case reflect.Array:
		if err := assertType(source, C.JANET_ARRAY); err != nil {
			return err
		}

		wantElements := type_.Len()
		haveElements := int(C.janet_length(source))
		if haveElements != wantElements {
			return fmt.Errorf("janet array had %d elements, wanted %d", haveElements, wantElements)
		}

		for i := 0; i < type_.Len(); i++ {
			value_ := C.janet_get(source, C.janet_wrap_integer(C.int(i)))
			err := v.unmarshal(value_, value.Index(i).Addr().Interface())
			if err != nil {
				return fmt.Errorf("failed to unmarshal array index %d: %s", i, err.Error())
			}
		}
	case reflect.Slice:
		if err := assertType(
			source,
			C.JANET_ARRAY,
			C.JANET_TUPLE); err != nil {
			return err
		}

		haveElements := int(C.janet_length(source))

		element := type_.Elem()
		slice := reflect.MakeSlice(type_, 0, 0)

		for i := 0; i < int(haveElements); i++ {
			value_ := C.janet_get(source, C.janet_wrap_integer(C.int(i)))
			entry := reflect.New(element)
			err := v.unmarshal(value_, entry.Interface())
			if err != nil {
				return fmt.Errorf("failed to unmarshal slice index %d: %s", i, err.Error())
			}
			slice = reflect.Append(slice, entry.Elem())
		}

		value.Set(slice)
	default:
		return fmt.Errorf("unimplemented type: %s (%s)", type_.String(), type_.Kind().String())
	}

	return nil
}

// unmarshalSafe sends an unmarshalRequest to the VM and waits for the result.
func (v *VM) unmarshalSafe(source C.Janet, dest interface{}) error {
	errc := make(chan error)
	v.requests <- unmarshalRequest{
		source: source,
		dest:   dest,
		errc:   errc,
	}
	return <-errc
}

// Marshal sends a marshalRequest to the VM and waits for the result.
func (v *VM) Marshal(source interface{}) (value *Value, err error) {
	result := make(chan interface{})
	v.requests <- marshalRequest{
		source: source,
		result: result,
	}

	res := <-result
	switch res := res.(type) {
	case *Value:
		return res, nil
	case error:
		return nil, res
	}

	return nil, fmt.Errorf("never got result of Marshal")
}
