package anim

import (
	"github.com/cfoust/cy/pkg/anim/collapse"
	"github.com/cfoust/cy/pkg/anim/conway"
	"github.com/cfoust/cy/pkg/anim/cos"
	"github.com/cfoust/cy/pkg/anim/cy"
	"github.com/cfoust/cy/pkg/anim/fluid"
	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/anim/midjo"
	"github.com/cfoust/cy/pkg/anim/musicforprogramming"
	"github.com/cfoust/cy/pkg/anim/perlin"
	"github.com/cfoust/cy/pkg/anim/slime"
)

const DEFAULT_FPS = 30

type Creator = meta.Creator
type Animation = meta.Animation

var Animations = map[string]Creator{}

func registerAnimation(name string, animation Creator) {
	Animations[name] = animation
}

func init() {
	registerAnimation("collapse", func() Animation {
		return &collapse.Collapse{}
	})
	registerAnimation("conway", func() Animation {
		return &conway.Conway{}
	})
	registerAnimation("cos", func() Animation {
		return &cos.Cos{}
	})
	registerAnimation("cy", func() Animation {
		return &cy.Cyform{}
	})
	registerAnimation("fluid", func() Animation {
		return &fluid.Fluid{}
	})
	registerAnimation("perlin", func() Animation {
		return perlin.New()
	})
	registerAnimation("midjo", func() Animation {
		return &midjo.Midjo{}
	})
	registerAnimation("musicforprogramming", func() Animation {
		return musicforprogramming.New()
	})
	registerAnimation("slime", func() Animation {
		return &slime.Slime{}
	})
}
