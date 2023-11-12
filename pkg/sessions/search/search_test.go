package search

import (
	"testing"

	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/sessions"

	"github.com/stretchr/testify/require"
)

func makeWrites(lines ...string) (events []sessions.Event) {
	for _, line := range lines {
		events = append(events, sessions.Event{
			Message: P.OutputMessage{
				Data: []byte(line),
			},
		})
	}
	return
}

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
	results, err := Search(
		makeWrites(
			"foo",
			"bar",
			"baz",
		),
		"bar",
		nil,
	)
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
		Selection: Selection{
			From: geom.Vec2{
				C: 3,
			},
			To: geom.Vec2{
				C: 6,
			},
		},
	}, results[0])
}
