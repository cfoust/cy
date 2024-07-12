// This is a simple executable that runs the Janet code provided on standard
// input in a test environment using a fake client.
package main

import (
	"context"
	"fmt"
	"io"
	"os"

	"github.com/cfoust/cy/pkg/cy"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"
)

func main() {
	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}

	server, create, err := cy.NewTestServer()
	if err != nil {
		panic(err)
	}

	client, err := create(geom.DEFAULT_SIZE)
	if err != nil {
		panic(err)
	}

	err = server.ExecuteCall(
		context.Background(),
		client,
		janet.CallBytes(
			data,
		),
	)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
