package main

import (
	"context"
	"fmt"
	"sort"
	"strings"
	_ "embed"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/cy"
	F "github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/janet"

	"github.com/alecthomas/kong"
)

//go:embed gen-api.janet
var GEN_API string

var CLI struct {
	API        struct{} `cmd:"" help:"Generate Markdown for the full API."`
	Frames     struct{} `cmd:"" help:"Generate Markdown for frames."`
	Animations struct{} `cmd:"" help:"Generate Markdown for animations."`
}

type Symbol struct {
	Name, Docstring, Link string
	Macro                 bool
}

func main() {
	ctx := kong.Parse(&CLI,
		kong.Name("cy-docs"),
		kong.Description("generate documentation for cy programmatically"),
		kong.UsageOnError(),
		kong.ConfigureHelp(kong.HelpOptions{
			Compact: true,
			Summary: true,
		}))

	switch ctx.Command() {
	case "api":
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

		err = cy.Execute(ctx, GEN_API)
		if err != nil {
			panic(err)
		}

		sort.SliceStable(symbols, func(i, j int) bool {
			return symbols[i].Name < symbols[j].Name
		})

		var output string

		// Generate the table of contents
		output += "## Functions\n\n"
		for _, symbol := range symbols {
			header := strings.Map(func(r rune) rune {
				if r == '/' || r == '?' {
					return -1
				}

				return r
			}, symbol.Name)

			output += fmt.Sprintf("[%s](#%s) ", symbol.Name, header)
		}

		output += "\n\n---\n"

		for _, symbol := range symbols {
			_type := "function"

			if symbol.Macro {
				_type = "macro"
			}

			source := ""
			if len(symbol.Link) > 0 {
				source = fmt.Sprintf("[source](%s)", symbol.Link)
			}

			lines := strings.Split(symbol.Docstring, "\n")

			if len(lines) == 0 {
				continue
			}

			first := "```janet\n" + lines[0] + "\n```"

			rest := ""
			if len(lines) > 1 {
				rest = "\n" + strings.Join(lines[1:], "\n")
			}

			output += fmt.Sprintf(`
#### %s

%s

%s%s

%s

---
`, symbol.Name, _type, first, rest, source)
		}

		fmt.Println(output)
	case "frames":
		frames := make([]string, 0)
		for name := range F.Frames {
			frames = append(frames, name)
		}
		sort.SliceStable(frames, func(i, j int) bool {
			return frames[i] < frames[j]
		})

		output := "\n---\n"

		for _, frame := range frames {
			set := fmt.Sprintf("```janet\n(viewport/set-frame \"%s\")\n```\n", frame)
			output += fmt.Sprintf(`
#### %s

%s

{{story png frame/%s}}

---
`, frame, set, frame)
		}

		fmt.Println(output)
	case "animations":
		animations := make([]string, 0)
		for name := range anim.Animations {
			animations = append(animations, name)
		}
		sort.SliceStable(animations, func(i, j int) bool {
			return animations[i] < animations[j]
		})

		output := "\n---\n"

		for _, animation := range animations {
			output += fmt.Sprintf(`
#### %s

{{story gif animation/%s}}

---
`, animation, animation)
		}

		fmt.Println(output)
	default:
		panic(ctx.Command())
	}
}
