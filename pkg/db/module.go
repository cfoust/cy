package db

import (
	"database/sql"
	"os"
	"time"

	"github.com/cfoust/cy/pkg/io"
)

// OpenOrCreate creates a new database at the given path using lock-based coordination.
// To avoid conflicts between multiple instances of cy, a lock file is created before
// the database is created. If the database already exists, it opens it instead.
//
// createFunc should create a new database with schema at the given path.
// openFunc should open an existing database at the given path.
func OpenOrCreate(
	dbPath string,
	createFunc func(string) (*sql.DB, error),
	openFunc func(string) (*sql.DB, error),
) (*sql.DB, error) {
	lockPath := dbPath + ".lock"
	for {
		if _, err := os.Stat(dbPath); !os.IsNotExist(err) {
			return openFunc(dbPath)
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

		return createFunc(dbPath)
	}
}
