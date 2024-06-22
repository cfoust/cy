package main

import (
	"context"
	_ "embed"
	"encoding/json"
	"os"
	"sort"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/cy"
	F "github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/janet"
)

//go:embed gen-api.janet
var GEN_API string

type Symbol struct {
	Name, Docstring, Link string
	Macro                 bool
}

type Binding struct {
	Source, Function string
	Sequence         []string
}

func main() {
	ctx := context.Background()
	cy, err := cy.Start(ctx, cy.Options{})
	if err != nil {
		panic(err)
	}

	err = cy.Callback("cy/env", "", func() *janet.Value {
		return cy.Env().Value
	})
	if err != nil {
		panic(err)
	}

	symbols := make([]Symbol, 0)
	cy.Callback("cy/doc", "", func(name string, docstring string, link string, macro bool) {
		symbols = append(symbols, Symbol{
			Name:      name,
			Docstring: docstring,
			Link:      link,
			Macro:     macro,
		})
	})

	binds := make([]Binding, 0)
	cy.Callback("cy/bind", "", func(source string, sequence []string, function string) {
		binds = append(binds, Binding{
			Source:   source,
			Sequence: sequence,
			Function: function,
		})
	})

	err = cy.Execute(ctx, GEN_API)
	if err != nil {
		panic(err)
	}

	sort.SliceStable(symbols, func(i, j int) bool {
		return symbols[i].Name < symbols[j].Name
	})

	frames := make([]string, 0)
	for name := range F.Frames {
		frames = append(frames, name)
	}
	sort.SliceStable(frames, func(i, j int) bool {
		return frames[i] < frames[j]
	})

	animations := make([]string, 0)
	for name := range anim.Animations {
		animations = append(animations, name)
	}
	sort.SliceStable(animations, func(i, j int) bool {
		return animations[i] < animations[j]
	})

	data, err := json.Marshal(map[string]interface{}{
		"Symbols":    symbols,
		"Binds":      binds,
		"Frames":     frames,
		"Animations": animations,
	})
	if err != nil {
		panic(err)
	}
	os.Stdout.Write(data)
}
