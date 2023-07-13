//go:build darwin
// +build darwin

package dir

// #cgo LDFLAGS:-lproc
// #include <libproc.h>
import "C"

import (
	"unsafe"
	"fmt"
)

const PROC_PIDVNODEPATHINFO = 9

type PathInfo struct {
	cdir C.struct_vnode_info_path
	rdir C.struct_vnode_info_path
}

func ForPid(pid int) (string, error) {
	var pathInfo PathInfo
	pathInfoPtr := unsafe.Pointer(&pathInfo)

	returnCode := int(C.proc_pidinfo(
		C.int(pid),
		PROC_PIDVNODEPATHINFO,
		0,
		pathInfoPtr,
		C.int(unsafe.Sizeof(pathInfo)),
	))

	if returnCode <= 0 {
		return "", fmt.Errorf("failed to call proc_pidinfo: %d", returnCode)
	}

	return C.GoString(&pathInfo.cdir.vip_path[0]), nil
}
