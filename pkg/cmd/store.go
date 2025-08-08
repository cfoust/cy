package cmd

import (
	"context"
	"os"
	"path"
	"time"

	"github.com/cfoust/cy/pkg/io"

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

// openDatabase creates a new command database at the given path. To avoid
// conflicts between multiple instances of cy, a lock file is created before
// the database is created.
func openDatabase(dbPath string) (*DB, error) {
	lockPath := dbPath + ".lock"
	for {
		if _, err := os.Stat(dbPath); !os.IsNotExist(err) {
			return Open(dbPath)
		}

		// DB does not exist, create lock file
		lock, err := io.Lock(lockPath)
		if err != nil {
			if err == io.ErrorLockFailed {
				time.Sleep(100 * time.Millisecond)
				continue
			}

			return nil, err
		}

		defer func() {
			_ = lock.Close()
			_ = os.Remove(lockPath)
		}()

		return Create(dbPath)
	}
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
