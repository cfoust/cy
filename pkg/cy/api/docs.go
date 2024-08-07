package api

import (
	_ "embed"

	"github.com/cfoust/cy/pkg/janet"
)

//go:embed docs-replay.md
var DOCS_REPLAY string

var _ janet.Documented = (*ReplayModule)(nil)

func (i *ReplayModule) Documentation() string {
	return DOCS_REPLAY
}

//go:embed docs-path.md
var DOCS_PATH string

var _ janet.Documented = (*PathModule)(nil)

func (i *PathModule) Documentation() string {
	return DOCS_PATH
}

//go:embed docs-input.md
var DOCS_INPUT string

var _ janet.Documented = (*InputModule)(nil)

func (i *InputModule) Documentation() string {
	return DOCS_INPUT
}

//go:embed docs-key.md
var DOCS_KEY string

var _ janet.Documented = (*KeyModule)(nil)

func (i *KeyModule) Documentation() string {
	return DOCS_KEY
}

//go:embed docs-group.md
var DOCS_GROUP string

var _ janet.Documented = (*GroupModule)(nil)

func (i *GroupModule) Documentation() string {
	return DOCS_GROUP
}

//go:embed docs-pane.md
var DOCS_PANE string

var _ janet.Documented = (*PaneModule)(nil)

func (i *PaneModule) Documentation() string {
	return DOCS_PANE
}

//go:embed docs-tree.md
var DOCS_TREE string

var _ janet.Documented = (*TreeModule)(nil)

func (i *TreeModule) Documentation() string {
	return DOCS_TREE
}

//go:embed docs-viewport.md
var DOCS_VIEWPORT string

var _ janet.Documented = (*ViewportModule)(nil)

func (i *ViewportModule) Documentation() string {
	return DOCS_VIEWPORT
}

//go:embed docs-cmd.md
var DOCS_CMD string

var _ janet.Documented = (*CmdModule)(nil)

func (i *CmdModule) Documentation() string {
	return DOCS_CMD
}

//go:embed docs-exec.md
var DOCS_EXEC string

var _ janet.Documented = (*ExecModule)(nil)

func (i *ExecModule) Documentation() string {
	return DOCS_EXEC
}

//go:embed docs-param.md
var DOCS_PARAM string

var _ janet.Documented = (*ParamModule)(nil)

func (p *ParamModule) Documentation() string {
	return DOCS_PARAM
}

//go:embed docs-msg.md
var DOCS_MSG string

var _ janet.Documented = (*MsgModule)(nil)

func (m *MsgModule) Documentation() string {
	return DOCS_MSG
}

//go:embed docs-layout.md
var DOCS_LAYOUT string

var _ janet.Documented = (*LayoutModule)(nil)

func (l *LayoutModule) Documentation() string {
	return DOCS_LAYOUT
}

//go:embed docs-style.md
var DOCS_STYLE string

var _ janet.Documented = (*StyleModule)(nil)

func (s *StyleModule) Documentation() string {
	return DOCS_STYLE
}
