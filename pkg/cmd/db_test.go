package cmd

import (
	"context"
	"database/sql"
	_ "github.com/mattn/go-sqlite3"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/db/cmd"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay/detect"
	"github.com/cfoust/cy/pkg/sessions/search"

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

func TestCommandCreate(t *testing.T) {
	db, err := openDB()
	require.NoError(t, err)
	defer db.Close()

	ctx := context.Background()
	command := detect.Command{
		Text: "ls",
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
		Prompted:  1,
		Executed:  2,
		Completed: 3,
	}

	err = db.CreateCommand(
		ctx,
		CommandEvent{
			Timestamp: time.Now(),
			Command:   command,
			Borg:      "foo.borg",
			Cwd:       "/tmp",
		},
	)
	require.NoError(t, err)

	err = db.CreateCommand(
		ctx,
		CommandEvent{
			Timestamp: time.Now(),
			Command:   command,
			Borg:      "foo2.borg",
			Cwd:       "/tmp",
		},
	)
	require.NoError(t, err)

	commands, err := db.ListCommands(ctx)
	require.NoError(t, err)
	require.Equal(t, 2, len(commands))
	require.Equal(t, command, commands[0].Command)
	require.Equal(t, command, commands[1].Command)
}
