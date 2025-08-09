package cmd

import (
	"context"
	"database/sql"
	"os"
	"path"

	"github.com/cfoust/cy/pkg/db"
	"github.com/cfoust/cy/pkg/db/cmd"

	"github.com/sasha-s/go-deadlock"
)

// Store coordinates the access to and creation of SQLite databases
// containing executed commands.
type Store struct {
	deadlock.RWMutex
	dbs map[string]*DB
}

const (
	DB_NAME = "cmd.db"
)

// OpenDatabases looks for cmd.db in all of the provided data directories and
// opens them if they exist. This method does not create new databases.
func (s *Store) OpenDatabases(dirs []string) error {
	for _, dir := range dirs {
		dbPath := path.Join(dir, DB_NAME)
		if _, err := os.Stat(dbPath); os.IsNotExist(err) {
			continue
		}

		s.RLock()
		_, ok := s.dbs[dbPath]
		s.RUnlock()
		if ok {
			continue
		}

		db, err := Open(dbPath)
		if err != nil {
			return err
		}

		s.Lock()
		s.dbs[dbPath] = db
		s.Unlock()
	}

	return nil
}

// QueryCommands queries commands across all open command databases.
func (s *Store) QueryCommands(ctx context.Context) ([]CommandEvent, error) {
	dbs := make(map[string]*DB)
	s.RLock()
	for path, db := range s.dbs {
		dbs[path] = db
	}
	s.RUnlock()

	allCommands := make([]CommandEvent, 0)
	for dbPath, db := range dbs {
		commands, err := db.ListCommands(ctx)
		if err != nil {
			return nil, err
		}

		// Make .borg path absolute
		for i, command := range commands {
			commands[i].Borg = path.Join(
				path.Dir(dbPath),
				command.Borg,
			)
		}

		allCommands = append(allCommands, commands...)
	}

	return allCommands, nil
}

func openDatabase(dbPath string) (*DB, error) {
	sqlDB, err := db.OpenOrCreate(
		dbPath,
		func(path string) (*sql.DB, error) {
			db, err := sql.Open("sqlite3", path)
			if err != nil {
				return nil, err
			}
			if _, err := db.Exec(cmd.SCHEMA); err != nil {
				_ = db.Close()
				return nil, err
			}
			return db, nil
		},
		func(path string) (*sql.DB, error) {
			return sql.Open("sqlite3", path)
		},
	)
	if err != nil {
		return nil, err
	}

	return newDB(sqlDB), nil
}

// SaveCommand stores a command in the command database adjacent to its borg
// file.
func (s *Store) SaveCommand(ctx context.Context, c CommandEvent) error {
	dbPath := path.Join(path.Dir(c.Borg), DB_NAME)

	s.RLock()
	db, ok := s.dbs[dbPath]
	s.RUnlock()

	// Make .borg path relative to db
	c.Borg = path.Base(c.Borg)

	if ok {
		return db.CreateCommand(ctx, c)
	}

	db, err := openDatabase(dbPath)
	if err != nil {
		return err
	}

	s.Lock()
	s.dbs[dbPath] = db
	s.Unlock()
	return db.CreateCommand(ctx, c)
}

func NewStore() *Store {
	return &Store{
		dbs: make(map[string]*DB),
	}
}
