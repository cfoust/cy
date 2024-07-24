package api

import (
	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/frames"
)

type ViewportModule struct {
}

func (c *ViewportModule) GetFrames(context interface{}) []string {
	var names []string
	for name := range frames.Frames {
		names = append(names, name)
	}
	return names
}

func (c *ViewportModule) GetAnimations(context interface{}) []string {
	var names []string
	for name := range anim.Animations {
		names = append(names, name)
	}
	return names
}
