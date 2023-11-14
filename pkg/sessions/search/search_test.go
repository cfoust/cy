package search

import (
	"testing"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"

	"github.com/stretchr/testify/require"
)

func TestGetPartial(t *testing.T) {
	re, err := getPartial("[asd]blah")
	require.NoError(t, err)
	require.Equal(t, "[ads]", re.String())

	re, err = getPartial("([asd]|[123])blah")
	require.NoError(t, err)
	require.Equal(t, "[1-3ads]", re.String())

	re, err = getPartial("([asd]*|[123]+)blah")
	require.NoError(t, err)
	require.Equal(t, "([ads]|[1-3])", re.String())
}

func TestBasic(t *testing.T) {
	sim := sessions.NewSimulator().
		Add(
			"foo",
			"bar",
			"baz",
		)
	results, err := Search(sim.Events(), "bar", nil)
	require.NoError(t, err)
	require.Equal(t, 1, len(results))
	require.Equal(t, SearchResult{
		Begin: Address{
			Index:  1,
			Offset: 2,
		},
		End: Address{
			Index:  2,
			Offset: 2,
		},
		Appearances: []Appearance{
			{
				Begin: Address{
					Index:  1,
					Offset: 2,
				},
				End: Address{
					Index:  2,
					Offset: 2,
				},
				Selection: Selection{
					From: geom.Vec2{
						C: 3,
					},
					To: geom.Vec2{
						C: 5,
					},
				},
			},
		},
	}, results[0])
}
