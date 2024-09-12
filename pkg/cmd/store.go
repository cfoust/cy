package cmd

import (
	"context"
	"os"
	"path"

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

// QueryCommands queries commands across all open command databases.
func (s *Store) QueryCommands(ctx context.Context) ([]CommandEvent, error) {
	dbs := make([]*DB, 0, len(s.dbs))
	s.RLock()
	for _, db := range s.dbs {
		dbs = append(dbs, db)
	}
	s.RUnlock()

	allCommands := make([]CommandEvent, 0)
	for _, db := range dbs {
		commands, err := db.ListCommands(ctx)
		if err != nil {
			return nil, err
		}
		allCommands = append(allCommands, commands...)
	}

	return allCommands, nil
}

// SaveCommand stores a command in the command database adjacent to its borg
// file.
func (s *Store) SaveCommand(ctx context.Context, c CommandEvent) error {
	dbPath := path.Join(path.Dir(c.Borg), DB_NAME)

	s.RLock()
	db, ok := s.dbs[dbPath]
	s.RUnlock()

	if ok {
		return db.CreateCommand(ctx, c)
	}

	// TODO(cfoust): 09/12/24 create lock file before creating db
	// TODO(cfoust): 09/12/24 index existing .borg files
	if _, err := os.Stat(dbPath); os.IsNotExist(err) {
		db, err = Create(dbPath)
		if err != nil {
			return err
		}
	} else {
		db, err = Open(dbPath)
		if err != nil {
			return err
		}
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
