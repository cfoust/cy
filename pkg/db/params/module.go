package params

import (
	"database/sql"
	_ "embed"
	"fmt"
	"os"

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

func newDB(db *sql.DB) *DB {
	return &DB{
		DB:      db,
		Queries: New(db),
	}
}

// initSchema creates the parameters table if it doesn't exist
func initSchema(db *sql.DB) error {
	_, err := db.Exec(SCHEMA)
	return err
}

func Create(filename string) (*DB, error) {
	db, err := sql.Open("sqlite3", filename+"?_foreign_keys=on")
	if err != nil {
		return nil, err
	}

	if err := initSchema(db); err != nil {
		_ = db.Close()
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

	db, err := sql.Open("sqlite3", filename+"?_foreign_keys=on")
	if err != nil {
		return nil, err
	}

	return newDB(db), nil
}
