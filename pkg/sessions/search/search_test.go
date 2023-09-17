package search

import (
	"testing"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"

	"github.com/stretchr/testify/require"
)

func makeWrites(lines ...string) (events []sessions.Event) {
	for _, line := range lines {
		events = append(events, sessions.Event{
			Message: sessions.OutputEvent{
				Bytes: []byte(line),
			},
		})
	}
	return
}

func TestBasic(t *testing.T) {
	results, _ := Search(
		makeWrites(
			"foo",
			"bar",
			"baz",
		),
		"^bar",
	)
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
		From: geom.Vec2{
			C: 3,
		},
		To: geom.Vec2{
			C: 6,
		},
	}, results[0])
}
