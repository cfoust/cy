// Code generated by sqlc. DO NOT EDIT.
// versions:
//   sqlc v1.27.0
// source: query.sql

package cmd

import (
	"context"
	"time"
)

const createCommand = `-- name: CreateCommand :one
INSERT INTO commands (
  executed_at,
  completed_at,
  session,
  text,
  directory,
  prompted,
  executed,
  completed
) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
RETURNING id
`

type CreateCommandParams struct {
	ExecutedAt  time.Time
	CompletedAt time.Time
	Session     int64
	Text        string
	Directory   string
	Prompted    int64
	Executed    int64
	Completed   int64
}

func (q *Queries) CreateCommand(ctx context.Context, arg CreateCommandParams) (int64, error) {
	row := q.db.QueryRowContext(ctx, createCommand,
		arg.ExecutedAt,
		arg.CompletedAt,
		arg.Session,
		arg.Text,
		arg.Directory,
		arg.Prompted,
		arg.Executed,
		arg.Completed,
	)
	var id int64
	err := row.Scan(&id)
	return id, err
}

const createSelection = `-- name: CreateSelection :exec
INSERT INTO selections (
  command,
  input,
  from_row,
  from_col,
  to_row,
  to_col
) VALUES (?, ?, ?, ?, ?, ?)
`

type CreateSelectionParams struct {
	Command int64
	Input   bool
	FromRow int64
	FromCol int64
	ToRow   int64
	ToCol   int64
}

func (q *Queries) CreateSelection(ctx context.Context, arg CreateSelectionParams) error {
	_, err := q.db.ExecContext(ctx, createSelection,
		arg.Command,
		arg.Input,
		arg.FromRow,
		arg.FromCol,
		arg.ToRow,
		arg.ToCol,
	)
	return err
}

const createSession = `-- name: CreateSession :one
INSERT INTO sessions (path)
VALUES (?)
ON CONFLICT DO NOTHING
RETURNING id
`

func (q *Queries) CreateSession(ctx context.Context, path string) (int64, error) {
	row := q.db.QueryRowContext(ctx, createSession, path)
	var id int64
	err := row.Scan(&id)
	return id, err
}

const getSession = `-- name: GetSession :one
SELECT id FROM sessions
WHERE path = ?
`

func (q *Queries) GetSession(ctx context.Context, path string) (int64, error) {
	row := q.db.QueryRowContext(ctx, getSession, path)
	var id int64
	err := row.Scan(&id)
	return id, err
}

const listCommands = `-- name: ListCommands :many
SELECT command.id,
       command.executed_at,
       command.completed_at,
       session.path,
       command.text,
       command.directory,
       command.prompted,
       command.executed,
       command.completed,
       selection.input,
       selection.from_row,
       selection.from_col,
       selection.to_row,
       selection.to_col
FROM commands command
  INNER JOIN sessions session ON command.session = session.id
  INNER JOIN selections selection ON command.id = selection.command
ORDER BY executed_at DESC
`

type ListCommandsRow struct {
	ID          int64
	ExecutedAt  time.Time
	CompletedAt time.Time
	Path        string
	Text        string
	Directory   string
	Prompted    int64
	Executed    int64
	Completed   int64
	Input       bool
	FromRow     int64
	FromCol     int64
	ToRow       int64
	ToCol       int64
}

func (q *Queries) ListCommands(ctx context.Context) ([]ListCommandsRow, error) {
	rows, err := q.db.QueryContext(ctx, listCommands)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var items []ListCommandsRow
	for rows.Next() {
		var i ListCommandsRow
		if err := rows.Scan(
			&i.ID,
			&i.ExecutedAt,
			&i.CompletedAt,
			&i.Path,
			&i.Text,
			&i.Directory,
			&i.Prompted,
			&i.Executed,
			&i.Completed,
			&i.Input,
			&i.FromRow,
			&i.FromCol,
			&i.ToRow,
			&i.ToCol,
		); err != nil {
			return nil, err
		}
		items = append(items, i)
	}
	if err := rows.Close(); err != nil {
		return nil, err
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	return items, nil
}

const listSessions = `-- name: ListSessions :many
SELECT id, path FROM sessions
ORDER BY path
`

func (q *Queries) ListSessions(ctx context.Context) ([]Session, error) {
	rows, err := q.db.QueryContext(ctx, listSessions)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var items []Session
	for rows.Next() {
		var i Session
		if err := rows.Scan(&i.ID, &i.Path); err != nil {
			return nil, err
		}
		items = append(items, i)
	}
	if err := rows.Close(); err != nil {
		return nil, err
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	return items, nil
}

const removeCommandSelections = `-- name: RemoveCommandSelections :exec
DELETE FROM selections WHERE command = ?
`

func (q *Queries) RemoveCommandSelections(ctx context.Context, command int64) error {
	_, err := q.db.ExecContext(ctx, removeCommandSelections, command)
	return err
}

const removeSession = `-- name: RemoveSession :exec
DELETE FROM sessions WHERE id = ?
`

func (q *Queries) RemoveSession(ctx context.Context, id int64) error {
	_, err := q.db.ExecContext(ctx, removeSession, id)
	return err
}

const removeSessionCommands = `-- name: RemoveSessionCommands :exec
DELETE FROM commands WHERE session = ?
`

func (q *Queries) RemoveSessionCommands(ctx context.Context, session int64) error {
	_, err := q.db.ExecContext(ctx, removeSessionCommands, session)
	return err
}
