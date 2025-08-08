package params

import (
	"database/sql"
	_ "embed"

	_ "github.com/mattn/go-sqlite3"
)

//go:embed schema.sql
var SCHEMA string

type DB struct {
	*sql.DB
	*Queries
}

func (db *DB) Close() error {
	return db.DB.Close()
}

func NewDB(db *sql.DB) *DB {
	return &DB{
		DB:      db,
		Queries: New(db),
	}
}
