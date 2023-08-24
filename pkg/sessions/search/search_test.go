package search

import (
	"testing"

	"github.com/cfoust/cy/pkg/sessions"
	"github.com/stretchr/testify/require"
)

func makeWrites(lines ...string) (events []sessions.Event) {
	for _, line := range lines {
		events = append(events, sessions.Event{
			Data: sessions.OutputEvent{
				Bytes: []byte(line),
			},
		})
	}
	return
}

func TestBasic(t *testing.T) {
	results, _ := Search(
		makeWrites(
			"blah",
			"test",
		),
		"^test",
	)
	require.Equal(t, 1, len(results))
	require.Equal(t, SearchResult{
		Index:  1,
		Offset: 3,
	}, results[0])
}
