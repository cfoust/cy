package params

import (
	"database/sql"
	"fmt"
	"os"
	"path/filepath"

	_ "github.com/mattn/go-sqlite3"
)

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

func OpenDBAt(dbPath string) (*DB, error) {
	dir := filepath.Dir(dbPath)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return nil, fmt.Errorf("failed to create directory %s: %w", dir, err)
	}

	db, err := sql.Open("sqlite3", dbPath+"?_foreign_keys=on")
	if err != nil {
		return nil, fmt.Errorf("failed to open database: %w", err)
	}

	if err := initSchema(db); err != nil {
		_ = db.Close()
		return nil, fmt.Errorf("failed to initialize schema: %w", err)
	}

	return newDB(db), nil
}

// initSchema creates the parameters table if it doesn't exist
func initSchema(db *sql.DB) error {
	schema := `
		CREATE TABLE IF NOT EXISTS parameters (
			key TEXT PRIMARY KEY,
			value BLOB NOT NULL,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
			updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
		);

		CREATE TRIGGER IF NOT EXISTS update_updated_at
		AFTER UPDATE ON parameters
		BEGIN
			UPDATE parameters SET updated_at = CURRENT_TIMESTAMP WHERE key = NEW.key;
		END;
	`
	_, err := db.Exec(schema)
	return err
}
