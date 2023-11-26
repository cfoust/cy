package main

import (
	"context"
	"fmt"
	"sort"
	"strings"

	"github.com/cfoust/cy/pkg/cy"
	"github.com/cfoust/cy/pkg/janet"

	"github.com/alecthomas/kong"
)

import _ "embed"

//go:embed gen-api.janet
var GEN_API string

var CLI struct {
	API struct {
	} `cmd:"" help:"Generate Markdown for the full API."`
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

		err = cy.Callback("cy/env", func() *janet.Value {
			return cy.Env().Value
		})
		if err != nil {
			panic(err)
		}

		symbols := make([]Symbol, 0)
		cy.Callback("cy/doc", func(name string, docstring string, link string, macro bool) {
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
		output += "## Index\n\n"
		for _, symbol := range symbols {
			header := strings.Map(func(r rune) rune {
				if r == '/' {
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
	default:
		panic(ctx.Command())
	}
}
