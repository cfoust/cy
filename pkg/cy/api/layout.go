package api

import (
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/layout"
)

type LayoutModule struct {
}

func (l *LayoutModule) Set(context interface{}, value *janet.Value) error {
	defer value.Free()
	var layout layout.Layout
	err := value.Unmarshal(&layout)
	if err != nil {
		return err
	}

	client, err := getClient(context)
	if err != nil {
		return err
	}

	return client.SetLayout(layout)
}

func (l *LayoutModule) Get(context interface{}) (*layout.Layout, error) {
	client, err := getClient(context)
	if err != nil {
		return nil, err
	}

	layout := client.GetLayout()
	return &layout, nil
}
