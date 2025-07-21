package params

import (
	"context"
	"os"
	"testing"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/db/params"
)

func TestPersistentStoreBasic(t *testing.T) {
	// Create a temporary directory for testing
	tmpDir, err := os.MkdirTemp("", "cy_params_test_*")
	if err != nil {
		t.Fatalf("failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Initialize Janet VM
	vm, err := janet.New(context.Background())
	if err != nil {
		t.Fatalf("failed to create Janet VM: %v", err)
	}

	// Create database in temp directory
	db, err := params.OpenDBAt(tmpDir + "/test.db")
	if err != nil {
		t.Fatalf("failed to create database: %v", err)
	}
	defer db.Close()

	// Create persistent store directly (not via NewPersistentStore)
	store := &PersistentStore{
		db: db,
		vm: vm,
	}

	// Test basic set/get functionality using Marshal instead
	testValue, err := vm.Marshal(3)
	if err != nil {
		t.Fatalf("failed to create test value: %v", err)
	}

	if testValue == nil {
		t.Fatalf("test value is nil")
	}

	// Test serialization
	data, err := store.serializeJanetValue(testValue)
	if err != nil {
		t.Fatalf("failed to serialize value: %v", err)
	}

	// Test deserialization 
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

	t.Logf("Basic persistent store test passed!")
}