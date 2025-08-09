-- name: GetParameter :one
SELECT value FROM parameters
WHERE key = ?;

-- name: SetParameter :exec
INSERT OR REPLACE INTO parameters (key, value)
VALUES (?, ?);
