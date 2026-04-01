package main

import (
	"fmt"
	"os"

	"github.com/cfoust/cy/pkg/cy"
)

func testCommand() error {
	files := CLI.Test.Files
	if len(files) == 0 {
		config, ok := cy.FindConfig()
		if !ok {
			return fmt.Errorf(
				"no test files provided and no configuration file found",
			)
		}
		files = []string{config}
	}

	var allFailures []cy.TestFailure
	for _, file := range files {
		failures, err := cy.RunTestFile(file)
		if err != nil {
			return fmt.Errorf("%s: %s", file, err)
		}
		allFailures = append(allFailures, failures...)
	}

	for _, f := range allFailures {
		fmt.Fprintf(
			os.Stderr,
			"FAIL '%s': %v\n",
			f.Name,
			f.Error,
		)
	}

	if len(allFailures) > 0 {
		return fmt.Errorf(
			"%d test(s) failed",
			len(allFailures),
		)
	}

	return nil
}
