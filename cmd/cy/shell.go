package main

// #include <pwd.h>
// #include <stdlib.h>
import "C"

import (
	"fmt"
)

func getUserShell(uid int) (string, error) {
	passwd := C.getpwuid(C.uid_t(uid))
	if passwd == nil {
		return "", fmt.Errorf("failed to read passwd")
	}

	return C.GoString(passwd.pw_shell), nil
}
