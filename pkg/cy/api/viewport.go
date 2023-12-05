package api

import (
	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/geom"
)

type ViewportModule struct {
}

func (c *ViewportModule) Size(context interface{}) *geom.Vec2 {
	client, ok := context.(Client)
	if !ok {
		return nil
	}
	size := client.Margins().Size()
	return &size
}

func (c *ViewportModule) SetSize(context interface{}, size geom.Size) {
	client, ok := context.(Client)
	if !ok {
		return
	}
	client.Margins().SetSize(size)
}

func (c *ViewportModule) SetFrame(context interface{}, name string) {
	client, ok := context.(Client)
	if !ok {
		return
	}

	frame, ok := frames.Frames[name]
	if !ok {
		return
	}
	client.Frame().Set(frame)
}

func (c *ViewportModule) GetFrames(context interface{}) []string {
	var names []string
	for name := range frames.Frames {
		names = append(names, name)
	}
	return names
}
