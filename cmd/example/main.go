// This is a simple executable that runs the Janet code provided on standard
// input in a test environment using a fake client.
package main

import (
	"bufio"
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

	reader := bufio.NewReader(os.Stdin)

	_, _ = fmt.Fprintln(os.Stdout, "ready")

	for {
		client, err := create(geom.DEFAULT_SIZE)
		if err != nil {
			panic(err)
		}

		data, err := reader.ReadBytes(0)
		if err == io.EOF {
			os.Exit(0)
			return
		}
		if err != nil {
			panic(err)
		}

		// Trim the null byte delimiter
		code := data[:len(data)-1]

		call := janet.CallBytes(code)
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
