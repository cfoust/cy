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
	"unsafe"
	"strings"
)

// Marshal a Go value into a Janet value.
func marshal(item interface{}) (result C.Janet, err error) {
	result = C.janet_wrap_nil()

	type_ := reflect.TypeOf(item)
	value := reflect.ValueOf(item)

	if value.Kind() == reflect.Pointer {
		// TODO(cfoust): 06/23/23 maybe support this someday
		err = fmt.Errorf("cannot marshal pointer to value")
		return
	}

	switch type_.Kind() {
	case reflect.Int32, reflect.Int, reflect.Uint32:
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
		result = C.janet_wrap_string(C.janet_cstring(strPtr))
		defer C.free(unsafe.Pointer(strPtr))
	//case reflect.Struct:
	//err = marshalStruct(p, type_, value)
	//if err != nil {
	//return
	//}
	default:
		err = fmt.Errorf("unimplemented type: %s", type_.String())
		return
	}

	return
}

func unmarshal(source C.Janet, dest interface{}) error {
	type_ := reflect.TypeOf(dest)
	value := reflect.ValueOf(dest)

	if value.Kind() != reflect.Pointer {
		return fmt.Errorf("cannot unmarshal into non-pointer value")
	}

	type_ = type_.Elem()
	value = value.Elem()

	switch type_.Kind() {
	case reflect.Int32, reflect.Int, reflect.Uint32:
		if C.janet_checktype(source, C.JANET_NUMBER) == 0 {
			return fmt.Errorf("expected number")
		}
		unwrapped := C.janet_unwrap_integer(source)
		value.SetInt(int64(unwrapped))
	case reflect.Float64:
		if C.janet_checktype(source, C.JANET_NUMBER) == 0 {
			return fmt.Errorf("expected number")
		}
		unwrapped := C.janet_unwrap_number(source)
		value.SetFloat(float64(unwrapped))
	case reflect.Bool:
		if C.janet_checktype(source, C.JANET_BOOLEAN) == 0 {
			return fmt.Errorf("expected boolean")
		}
		unwrapped := C.janet_unwrap_boolean(source)
		if unwrapped == 1 {
			value.SetBool(true)
		} else {
			value.SetBool(false)
		}
	case reflect.String:
		if C.janet_checktype(source, C.JANET_STRING) == 0 {
			return fmt.Errorf("expected string")
		}
		strPtr := C.GoString(C.cast_janet_string(C.janet_unwrap_string(source)))
		value.SetString(strings.Clone(strPtr))
	default:
		return fmt.Errorf("unimplemented type: %s", type_.String())
	}

	return nil
}
