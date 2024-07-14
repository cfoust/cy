//go:build ignore

package main

import (
	"go/ast"
	"go/build"
	"go/doc"
	"go/parser"
	"go/token"
	"io/fs"
	"sort"
	"strings"
	"unicode"

	"github.com/rs/zerolog/log"
)

func trim(s string) string {
	return strings.TrimFunc(s, unicode.IsSpace)
}

func normalizeComment(comment *ast.Comment) (lines []string) {
	text := comment.Text

	if text[1] == '/' {
		return []string{trim(text[2:])}
	}

	lines = strings.Split(text, "\n")
	for i, line := range lines {
		switch i {
		case 0:
			// Remove /*
			if len(line) == 2 {
				line = ""
				continue
			}
			line = line[2:]
		case len(lines) - 1:
			if len(line) == 2 {
				continue
			}
			// Remove */
			line = line[:len(line)-2]
		}

		lines[i] = trim(line)
	}
	return
}

type Param struct {
	Name      string
	Docstring string
}

func main() {
	pkg, err := build.ImportDir(".", build.ImportComment)
	if err != nil {
		panic(err)
	}

	fset := token.NewFileSet()
	include := func(info fs.FileInfo) bool {
		return info.Name() == "defaults.go"
	}
	pkgs, err := parser.ParseDir(fset, pkg.Dir, include, parser.ParseComments)
	if err != nil {
		panic(err)
	}

	var params []Param

	docPkg := doc.New(pkgs["params"], pkg.ImportPath, doc.AllDecls)
	for _, typ := range docPkg.Types {
		for _, spec := range typ.Decl.Specs {
			typeSpec, ok := spec.(*ast.TypeSpec)
			if !ok {
				continue
			}

			structType, ok := typeSpec.Type.(*ast.StructType)
			if !ok {
				continue
			}

			for _, field := range structType.Fields.List {

				var docs []string
				for _, comment := range field.Doc.List {
					docs = append(docs, normalizeComment(comment)...)
				}

				params = append(params, Param{
					Name:      field.Names[0].Name,
					Docstring: strings.Join(docs, "\n"),
				})
			}
		}
	}

	sort.SliceStable(params, func(i, j int) bool {
		return params[i].Name < params[j].Name
	})

	log.Info().Msgf("%+v", params)
}
