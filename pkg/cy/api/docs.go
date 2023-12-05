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
