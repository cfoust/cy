package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/layout"
)

type LayoutModule struct {
}

func (l *LayoutModule) Set(user interface{}, value *janet.Value) error {
	defer value.Free()
	var layout layout.Layout
	err := value.Unmarshal(&layout)
	if err != nil {
		return err
	}

	client, ok := user.(Client)
	if !ok {
		return fmt.Errorf("missing client context")
	}

	return client.Layout().Set(layout)
}

func (l *LayoutModule) Get(user interface{}) (*layout.Layout, error) {
	client, ok := user.(Client)
	if !ok {
		return nil, fmt.Errorf("missing client context")
	}

	layout := client.Layout().Get()
	return &layout, nil
}
