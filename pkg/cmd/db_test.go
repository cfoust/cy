package cmd

import (
	"context"
	"database/sql"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/db/cmd"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay/detect"
	"github.com/cfoust/cy/pkg/sessions/search"

	_ "github.com/mattn/go-sqlite3"
	"github.com/stretchr/testify/require"
)

func openDB() (*DB, error) {
	db, err := sql.Open("sqlite3", ":memory:")
	if err != nil {
		return nil, err
	}

	if _, err := db.Exec(cmd.SCHEMA); err != nil {
		return nil, err
	}

	return newDB(db), nil
}

// createTestCommand creates a CommandEvent with the provided text. The other
// fields are set to placeholder values.
func createTestCommand(text string) CommandEvent {
	command := detect.Command{
		Text: text,
		Input: []search.Selection{
			{
				From: geom.Vec2{R: 1, C: 1},
				To:   geom.Vec2{R: 2, C: 2},
			},
		},
		Output: search.Selection{
			From: geom.Vec2{R: 3, C: 3},
			To:   geom.Vec2{R: 4, C: 4},
		},
		Prompted:    1,
		Executed:    2,
		ExecutedAt:  time.Now().UTC(),
		Completed:   3,
		CompletedAt: time.Now().UTC(),
		Directory:   "/tmp",
	}

	return CommandEvent{
		Command: command,
		Borg:    "foo.borg",
	}
}

func TestCommandCreate(t *testing.T) {
	db, err := openDB()
	require.NoError(t, err)
	defer func() { _ = db.Close() }()

	ctx := context.Background()

	c1 := createTestCommand("ls")
	c2 := createTestCommand("ls")
	c2.Borg = "foo2.borg"

	err = db.CreateCommand(ctx, c1)
	require.NoError(t, err)

	err = db.CreateCommand(ctx, c2)
	require.NoError(t, err)

	commands, err := db.ListCommands(ctx)
	require.NoError(t, err)
	require.Equal(t, 2, len(commands))

	// zero out timestamps for comparison. this is because time.Time's
	// internal representation is not stable across
	// serialization/deserialization
	c1.ExecutedAt = time.Time{}
	c1.CompletedAt = time.Time{}
	c2.ExecutedAt = time.Time{}
	c2.CompletedAt = time.Time{}
	for i := range commands {
		commands[i].ExecutedAt = time.Time{}
		commands[i].CompletedAt = time.Time{}
	}

	require.Equal(t, c1.Command, commands[0].Command)
	require.Equal(t, c2.Command, commands[1].Command)
}
