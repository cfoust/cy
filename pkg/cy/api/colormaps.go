package api

import (
	"github.com/cfoust/cy/pkg/style/colormaps"
)

type ColorMapsModule struct{}

func (c *ColorMapsModule) GetAll() []colormaps.Scheme {
	return colormaps.Schemes
}
