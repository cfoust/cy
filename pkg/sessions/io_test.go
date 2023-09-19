package sessions

import (
	P "github.com/cfoust/cy/pkg/io/protocol"
	"path/filepath"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

func TestReadWrite(t *testing.T) {
	name := filepath.Join(t.TempDir(), "foo.borg")
	w, err := Create(name)
	require.NoError(t, err)

	events := []Event{
		{
			Stamp: time.Unix(1, 2).UTC(),
			Message: P.OutputMessage{
				Data: []byte("test"),
			},
		},
		{
			Stamp: time.Unix(3, 4).UTC(),
			Message: P.SizeMessage{
				Rows:    2,
				Columns: 2,
			},
		},
		{
			Stamp: time.Unix(5, 6).UTC(),
			Message: P.OutputMessage{
				Data: []byte("test 2"),
			},
		},
	}

	for _, event := range events {
		require.NoError(t, w.Write(event))
	}

	require.NoError(t, w.Close())

	r, err := Open(name)
	require.NoError(t, err)

	for _, before := range events {
		after, err := r.Read()
		require.NoError(t, err)
		require.Equal(t, before, after)
	}
}
