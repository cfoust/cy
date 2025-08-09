package params

import (
	"testing"

	"github.com/cfoust/cy/pkg/janet"

	"github.com/stretchr/testify/require"
)

func TestPersistentStore(t *testing.T) {
	vm, err := janet.New(t.Context())
	require.NoError(t, err)

	db, err := Create(t.TempDir() + "/test.db")
	require.NoError(t, err)
	defer func() { _ = db.Close() }()

	store := &PersistentStore{
		db: db,
		vm: vm,
	}

	testValue, err := vm.Marshal(3)
	require.NoError(t, err, "failed to create test value")
	require.NotNil(t, testValue, "test value is nil")

	data, err := store.serializeJanetValue(testValue)
	require.NoError(t, err, "failed to serialize value")

	value, err := store.deserializeJanetValue(data)
	require.NoError(t, err, "failed to deserialize value")

	var result int
	err = value.Unmarshal(&result)
	require.NoError(t, err, "failed to unmarshal result")

	require.Equal(t, 3, result)
}
