package params

import (
	"testing"

	"github.com/cfoust/cy/pkg/db/params"
	"github.com/cfoust/cy/pkg/janet"
)

func TestPersistentStore(t *testing.T) {
	// Initialize Janet VM
	vm, err := janet.New(t.Context())
	if err != nil {
		t.Fatalf("failed to create Janet VM: %v", err)
	}

	// Create database in temp directory
	db, err := params.OpenDBAt(t.TempDir() + "/test.db")
	if err != nil {
		t.Fatalf("failed to create database: %v", err)
	}
	defer db.Close()

	store := &PersistentStore{
		db: db,
		vm: vm,
	}

	testValue, err := vm.Marshal(3)
	if err != nil {
		t.Fatalf("failed to create test value: %v", err)
	}

	if testValue == nil {
		t.Fatalf("test value is nil")
	}

	data, err := store.serializeJanetValue(testValue)
	if err != nil {
		t.Fatalf("failed to serialize value: %v", err)
	}

	value, err := store.deserializeJanetValue(data)
	if err != nil {
		t.Fatalf("failed to deserialize value: %v", err)
	}

	// Check that the value is correct
	var result int
	err = value.Unmarshal(&result)
	if err != nil {
		t.Fatalf("failed to unmarshal result: %v", err)
	}

	if result != 3 {
		t.Fatalf("expected 3, got %d", result)
	}
}
