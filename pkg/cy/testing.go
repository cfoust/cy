package cy

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
)

func NewTestServer() (*Cy, func(geom.Size) (*Client, error), error) {
	ctx := context.Background()
	cy, err := Start(ctx, Options{
		Shell:     "/bin/bash",
		SkipInput: true,
		StateDir:  "",
	})
	if err != nil {
		return nil, nil, err
	}

	return cy, func(size geom.Size) (*Client, error) {
		return cy.NewClient(ctx, ClientOptions{
			Env: map[string]string{
				"TERM": "xterm-256color",
			},
			Size: geom.DEFAULT_SIZE,
		})
	}, nil
}
