// Code generated by sqlc. DO NOT EDIT.
// versions:
//   sqlc v1.27.0

package cmd

import (
	"time"
)

type Command struct {
	ID          int64
	ExecutedAt  time.Time
	CompletedAt time.Time
	Session     int64
	Text        string
	Directory   string
	Prompted    int64
	Executed    int64
	Completed   int64
}

type Selection struct {
	ID      int64
	Command int64
	Input   bool
	FromRow int64
	FromCol int64
	ToRow   int64
	ToCol   int64
}

type Session struct {
	ID   int64
	Path string
}
