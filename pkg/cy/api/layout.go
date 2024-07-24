package api

import (
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/layout"
)

type LayoutModule struct {
}

func (l *LayoutModule) Set(value *janet.Value) error {
	defer value.Free()
	var layout layout.LayoutType
	return value.Unmarshal(&layout)
}
