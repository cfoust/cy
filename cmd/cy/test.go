package main

import (
	"fmt"
	"os"

	"github.com/cfoust/cy/pkg/cy"
)

func testCommand() error {
	var allFailures []cy.TestFailure
	for _, file := range CLI.Test.Files {
		failures, err := cy.RunTestFile(file)
		if err != nil {
			return fmt.Errorf("%s: %s", file, err)
		}
		allFailures = append(allFailures, failures...)
	}

	for _, f := range allFailures {
		fmt.Fprintf(
			os.Stderr,
			"FAIL %s: '%s': %v\n",
			f.File,
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

	fmt.Println("ok")
	return nil
}
