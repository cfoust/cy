package main

import (
	"os"
	"testing"

	"github.com/cfoust/cy/pkg/cy"

	"github.com/stretchr/testify/require"
)

func TestParse(t *testing.T) {
	_ = os.Setenv(cy.CONTEXT_ENV, "default:1")

	for index, test := range []struct {
		Value    string
		Expected *Reference
	}{
		{
			Value: "foobar",
		},
		{
			Value: "foo:2:3",
			Expected: &Reference{
				Socket: "foo",
				Node:   2,
				Index:  3,
			},
		},
		{
			Value: "2:3",
			Expected: &Reference{
				Socket: "default",
				Node:   2,
				Index:  3,
			},
		},
		{
			Value: "-1",
			Expected: &Reference{
				Socket: "default",
				Node:   1,
				Index:  -1,
			},
		},
	} {
		ref, err := parseReference(test.Value)
		if test.Expected == nil {
			require.Nil(t, ref, "test %d", index)
			require.Error(t, err, "test %d", index)
			continue
		}

		require.NoError(t, err, "test %d", index)
		require.Equal(t, test.Expected, ref, "test %d", index)
	}
}
