package cy

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/pty"
	"github.com/cfoust/cy/pkg/session"
	"github.com/cfoust/cy/pkg/util"
)

type Window struct {
	Name string

	pty     *pty.Pty
	session *session.Session
	done    chan error

	buffer *util.WaitBuffer

	Terminal emu.Terminal
}
