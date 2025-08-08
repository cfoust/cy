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
	server, create, err := cy.NewTestServer()
	if err != nil {
		panic(err)
	}

	buffer := make([]byte, 8192)

	_, _ = fmt.Fprintln(os.Stdout, "ready")

	for {
		client, err := create(geom.DEFAULT_SIZE)
		if err != nil {
			panic(err)
		}

		n, err := os.Stdin.Read(buffer)
		if err == io.EOF {
			os.Exit(0)
			return
		}
		if err != nil {
			panic(err)
		}

		call := janet.CallBytes(buffer[:n])
		call.Options.UpdateEnv = false
		_, err = server.ExecuteCall(
			context.Background(),
			client,
			call,
		)
		client.Cancel()
		if err != nil {
			_, _ = fmt.Fprintf(os.Stdout, "%+v\n", err)
			_, _ = fmt.Fprintf(os.Stdout, "error\n")
			continue
		}

		_, _ = fmt.Fprintf(os.Stdout, "ok\n")
	}
}
