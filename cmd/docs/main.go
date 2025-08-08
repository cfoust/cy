package main

import (
	"context"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"sort"
	"strings"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/cy"
	F "github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/janet"
	P "github.com/cfoust/cy/pkg/params"
)

//go:embed gen-api.janet
var GEN_API string

type Symbol struct {
	Name, Docstring, Link string
	Macro                 bool
}

type Binding struct {
	Tag, Source, Function string
	Sequence              []string
}

type Param struct {
	Name, Type, Default, Docstring string
}

func getParams() (params []Param, err error) {
	var vm *janet.VM
	vm, err = janet.New(context.Background())
	if err != nil {
		return
	}

	var handlerFunc *janet.Function
	_ = vm.Callback("handler", "", func(f *janet.Function) {
		handlerFunc = f
	})

	_ = vm.Callback("register", "", func(
		name string,
		docstring string,
		def string,
		typ string,
	) {
		if def == "" {
			def = "\"\""
		}

		params = append(params, Param{
			Name:      name,
			Docstring: docstring,
			Default:   def,
			Type:      typ,
		})
	})

	err = vm.Execute(context.Background(), `
(handler (fn [param]
	(def {:name name :docstring doc :default default} param)
	(register name doc (if (string? default) (string "\"" default "\"") (string/format "%n" default)) (string (type default)))
	))
`)
	if err != nil {
		return
	}

	for _, param := range P.DefaultParams() {
		err = handlerFunc.Call(
			context.Background(),
			param,
		)
		if err != nil {
			return
		}
	}

	return
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
	_ = cy.Callback("cy/doc", "", func(name string, docstring string, link string, macro bool) {
		if name == "cy/env" {
			return
		}

		symbols = append(symbols, Symbol{
			Name:      name,
			Docstring: docstring,
			Link:      link,
			Macro:     macro,
		})
	})

	binds := make([]Binding, 0)
	_ = cy.Callback("cy/bind", "", func(source string, tag string, sequence []string, function string) {
		binds = append(binds, Binding{
			Tag:      tag,
			Source:   source,
			Sequence: sequence,
			Function: function,
		})
	})

	err = cy.Execute(ctx, GEN_API)
	if err != nil {
		panic(err)
	}

	for i, symbol := range symbols {
		if len(symbol.Link) > 0 {
			continue
		}
		callback, ok := cy.Lookup(symbol.Name)
		if !ok {
			continue
		}
		file, line := callback.Source()
		index := strings.Index(file, "pkg")
		if index == -1 {
			continue
		}
		symbols[i].Link = fmt.Sprintf(
			"https://github.com/cfoust/cy/blob/main/%s#L%d",
			file[index:],
			line,
		)
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

	params, err := getParams()
	if err != nil {
		panic(err)
	}

	data, err := json.Marshal(map[string]interface{}{
		"Animations": animations,
		"Binds":      binds,
		"Frames":     frames,
		"Parameters": params,
		"Symbols":    symbols,
	})
	if err != nil {
		panic(err)
	}
	_, _ = os.Stdout.Write(data)
}
