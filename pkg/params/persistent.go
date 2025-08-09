package params

import (
	"context"
	"database/sql"
	"fmt"
	"os"
	"path/filepath"

	"github.com/cfoust/cy/pkg/db/params"
	"github.com/cfoust/cy/pkg/janet"

	"github.com/sasha-s/go-deadlock"
)

// PersistentStore provides persistent parameter storage using SQLite
type PersistentStore struct {
	deadlock.RWMutex
	db *params.DB
	vm *janet.VM
}

// NewPersistentStore creates a new persistent parameter store in the given
// state directory. If stateDir is empty, uses an in-memory database.
func NewPersistentStore(
	vm *janet.VM,
	stateDir string,
) (*PersistentStore, error) {
	var db *params.DB
	var err error

	if stateDir == "" {
		db, err = params.Create(":memory:")
	} else {
		dbPath := filepath.Join(stateDir, "params.db")

		if _, err = os.Stat(dbPath); os.IsNotExist(err) {
			db, err = params.Create(dbPath)
		} else {
			db, err = params.Open(dbPath)
		}
	}

	if err != nil {
		return nil, fmt.Errorf(
			"failed to initialize params database: %w",
			err,
		)
	}

	return &PersistentStore{
		db: db,
		vm: vm,
	}, nil
}

// serializeJanetValue converts a Janet value to binary using Janet's marshaling
func (p *PersistentStore) serializeJanetValue(
	value *janet.Value,
) ([]byte, error) {
	bytes, err := value.Bytes()
	if err != nil {
		return nil, fmt.Errorf("failed to marshal Janet value: %w", err)
	}

	return bytes, nil
}

// deserializeJanetValue converts binary data back to a Janet value
func (p *PersistentStore) deserializeJanetValue(
	data []byte,
) (*janet.Value, error) {
	if len(data) == 0 {
		return nil, fmt.Errorf("empty data cannot be deserialized")
	}

	value, err := p.vm.FromBytes(data)
	if err != nil {
		return nil, fmt.Errorf("failed to unmarshal Janet value: %w", err)
	}

	return value, nil
}

// Set stores a parameter value in the persistent store
func (p *PersistentStore) Set(
	ctx context.Context,
	key string,
	value *janet.Value,
) error {
	p.Lock()
	defer p.Unlock()

	data, err := p.serializeJanetValue(value)
	if err != nil {
		return fmt.Errorf("failed to serialize Janet value: %w", err)
	}

	err = p.db.SetParameter(ctx, params.SetParameterParams{
		Key:   key,
		Value: data,
	})
	if err != nil {
		return fmt.Errorf("failed to store parameter: %w", err)
	}

	return nil
}

// Get retrieves a parameter value from the persistent store
func (p *PersistentStore) Get(
	ctx context.Context,
	key string,
) (*janet.Value, bool, error) {
	p.RLock()
	defer p.RUnlock()

	data, err := p.db.GetParameter(ctx, key)
	if err == sql.ErrNoRows {
		return nil, false, nil
	}
	if err != nil {
		return nil, false, fmt.Errorf("failed to retrieve parameter: %w", err)
	}

	value, err := p.deserializeJanetValue(data)
	if err != nil {
		return nil, false, fmt.Errorf(
			"failed to deserialize Janet value: %w",
			err,
		)
	}

	return value, true, nil
}
