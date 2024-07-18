package cy

import (
	"github.com/cfoust/cy/pkg/janet"
)

import _ "embed"

type CyModule struct {
	cy *Cy
}

var _ janet.Documented = (*CyModule)(nil)

//go:embed docs-cy.md
var DOCS_CY string

func (i *CyModule) Documentation() string {
	return DOCS_CY
}

func (c *CyModule) KillServer() {
	c.cy.Shutdown()
}

func (c *CyModule) Detach(user interface{}) {
	client, ok := user.(*Client)
	if !ok {
		return
	}

	client.Detach("detached")
}

func (c *CyModule) ReloadConfig() error {
	return c.cy.reloadConfig()
}

func (c *CyModule) Paste(user interface{}) {
	client, ok := user.(*Client)
	if !ok {
		return
	}

	buffer := client.buffer
	if len(buffer) == 0 {
		return
	}

	client.binds.Input([]byte(buffer))
}
