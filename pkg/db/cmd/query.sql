-- name: ListCommands :many
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
ORDER BY executed_at ASC;

-- name: ListSessions :many
SELECT * FROM sessions
ORDER BY path;

-- name: GetSession :one
SELECT id FROM sessions
WHERE path = ?;

-- name: CreateSession :one
INSERT INTO sessions (path)
VALUES (?)
ON CONFLICT DO NOTHING
RETURNING id;

-- name: RemoveSession :exec
DELETE FROM sessions WHERE id = ?;

-- name: RemoveSessionCommands :exec
DELETE FROM commands WHERE session = ?;

-- name: RemoveCommandSelections :exec
DELETE FROM selections WHERE command = ?;

-- name: CreateSelection :exec
INSERT INTO selections (
  command,
  input,
  from_row,
  from_col,
  to_row,
  to_col
) VALUES (?, ?, ?, ?, ?, ?);

-- name: CreateCommand :one
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
RETURNING id;
