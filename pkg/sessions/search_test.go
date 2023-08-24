package sessions

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func makeWrites(lines ...string) (events []Event) {
	for _, line := range lines {
		events = append(events, Event{
			Data: OutputEvent{
				Bytes: []byte(line),
			},
		})
	}
	return
}

func TestBasic(t *testing.T) {
	results := Search(
		makeWrites(
			"blah",
			"test",
		),
		"test",
	)

	require.Equal(t, 1, len(results))
}
