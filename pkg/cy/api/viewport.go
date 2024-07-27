package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/frames"
)

type ViewportModule struct {
}

func (c *ViewportModule) SetFrame(context interface{}, name string) error {
	client, err := getClient(context)
	if err != nil {
		return err
	}

	frame, ok := frames.Frames[name]
	if !ok {
		return fmt.Errorf("could not find frame '%s'", name)
	}
	client.Frame().Set(frame)
	return nil
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
