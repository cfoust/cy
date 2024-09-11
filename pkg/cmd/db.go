package cmd

import (
	"context"
	"database/sql"
	"fmt"
	"os"

	"github.com/cfoust/cy/pkg/db/cmd"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/replay/detect"
	"github.com/cfoust/cy/pkg/sessions/search"
)

type DB struct {
	*sql.DB
	*cmd.Queries
}

func (db *DB) Close() error {
	return db.DB.Close()
}

func newDB(db *sql.DB) *DB {
	return &DB{
		DB:      db,
		Queries: cmd.New(db),
	}
}

func (db *DB) CreateCommand(
	ctx context.Context,
	c CommandEvent,
) error {
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		return err
	}

	defer tx.Rollback()

	queries := db.WithTx(tx)

	if len(c.Borg) == 0 {
		return fmt.Errorf("borg file name cannot be empty")
	}

	sessionId, err := queries.CreateSession(ctx, c.Borg)
	if err != nil {
		return err
	}

	commandId, err := queries.CreateCommand(
		ctx,
		cmd.CreateCommandParams{
			Timestamp: c.Timestamp,
			Session:   sessionId,
			Text:      c.Text,
			Directory: c.Cwd,
			Prompted:  int64(c.Prompted),
			Executed:  int64(c.Executed),
			Completed: int64(c.Completed),
		},
	)
	if err != nil {
		return err
	}

	for _, input := range c.Input {
		err := queries.CreateSelection(
			ctx,
			cmd.CreateSelectionParams{
				Command: commandId,
				Input:   true,
				FromRow: int64(input.From.R),
				FromCol: int64(input.From.C),
				ToRow:   int64(input.To.R),
				ToCol:   int64(input.To.C),
			},
		)
		if err != nil {
			return err
		}
	}

	err = queries.CreateSelection(
		ctx,
		cmd.CreateSelectionParams{
			Command: commandId,
			Input:   false,
			FromRow: int64(c.Output.From.R),
			FromCol: int64(c.Output.From.C),
			ToRow:   int64(c.Output.To.R),
			ToCol:   int64(c.Output.To.C),
		},
	)
	if err != nil {
		return err
	}

	if err = tx.Commit(); err != nil {
		return err
	}

	return err
}

// commandRowsToEvents converts a database rows returned by command queries
// into CommandEvents.
func commandRowsToEvents(rows []cmd.ListCommandsRow) (events []CommandEvent) {
	// Since there can be many input selections, we don't know for sure
	// how many commands there are
	events = make([]CommandEvent, 0)
	id := -1
	last := 0
	for _, row := range rows {
		if int(row.ID) != id {
			id = int(row.ID)
			events = append(events, CommandEvent{
				Timestamp: row.Timestamp,
				Borg:      row.Path,
				Cwd:       row.Directory,
				Command: detect.Command{
					Text:      row.Text,
					Prompted:  int(row.Prompted),
					Executed:  int(row.Executed),
					Completed: int(row.Completed),
				},
			})
			last = len(events) - 1
		}

		if row.Input {
			events[last].Input = append(events[last].Input, search.Selection{
				From: geom.Vec2{
					R: int(row.FromRow),
					C: int(row.FromCol),
				},
				To: geom.Vec2{
					R: int(row.ToRow),
					C: int(row.ToCol),
				},
			})
			continue
		}

		events[last].Output = search.Selection{
			From: geom.Vec2{
				R: int(row.FromRow),
				C: int(row.FromCol),
			},
			To: geom.Vec2{
				R: int(row.ToRow),
				C: int(row.ToCol),
			},
		}
	}
	return
}

func (db *DB) ListCommands(ctx context.Context) ([]CommandEvent, error) {
	rows, err := db.Queries.ListCommands(ctx)
	if err != nil {
		return nil, err
	}

	return commandRowsToEvents(rows), nil
}

func Create(filename string) (*DB, error) {
	db, err := sql.Open("sqlite3", filename)
	if err != nil {
		return nil, err
	}

	if _, err := db.Exec(cmd.SCHEMA); err != nil {
		return nil, err
	}

	return newDB(db), nil
}

func Open(filename string) (*DB, error) {
	if _, err := os.Stat(filename); os.IsNotExist(err) {
		return nil, fmt.Errorf(
			"%s does not exist",
			filename,
		)
	}

	db, err := sql.Open("sqlite3", filename)
	if err != nil {
		return nil, err
	}

	return newDB(db), nil
}
