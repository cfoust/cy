package main

import (
	"context"
	_ "embed"
	"fmt"
	"os"

	"github.com/cfoust/cy/pkg/janet"
)

//go:embed fmt.janet
var fmtJanet []byte

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: janet-format <file>...")
		os.Exit(1)
	}

	ctx := context.Background()
	vm, err := janet.New(ctx)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to create VM: %v\n", err)
		os.Exit(1)
	}

	// Load the formatter
	call := janet.CallBytes(fmtJanet)
	call.SourcePath = "fmt.janet"
	_, err = vm.ExecuteCall(ctx, nil, call)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to load formatter: %v\n", err)
		os.Exit(1)
	}

	// Format each file with custom indent-2 forms for cy macros
	for _, path := range os.Args[1:] {
		call := janet.CallString(
			fmt.Sprintf(`(format-file %q)`, path),
		)
		call.Options.Dyns = map[janet.Keyword]any{
			"user-indent-2-forms": []string{
				"key/action",
				"test",
				"test-no-context",
				"expect-error",
			},
		}
		_, err = vm.ExecuteCall(ctx, nil, call)
		if err != nil {
			fmt.Fprintf(
				os.Stderr,
				"error formatting %s: %v\n",
				path,
				err,
			)
			os.Exit(1)
		}
	}
}
