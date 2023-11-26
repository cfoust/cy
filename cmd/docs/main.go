package main

import (
	"context"
	"fmt"

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

		cy.Callback("cy/doc", func(name string, docstring string, link string, macro bool) {
			fmt.Printf("%s %s %t\n", name, docstring, macro)
		})

		err = cy.Execute(ctx, GEN_API)
		if err != nil {
			panic(err)
		}
	default:
		panic(ctx.Command())
	}
}
