package cmd

import (
	"context"
	"os"
	"path"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestStore(t *testing.T) {
	ctx := context.Background()
	store := NewStore()

	tempDir := t.TempDir()
	c := createTestCommand("test")
	c.Borg = path.Join(tempDir, "test.borg")

	if err := store.SaveCommand(ctx, c); err != nil {
		t.Fatal(err)
	}

	// Ensure DB exists
	dbPath := path.Join(tempDir, DB_NAME)
	if _, err := os.Stat(dbPath); os.IsNotExist(err) {
		t.Fatal(err)
	}

	// Want to make sure that the refreshing functionality works
	newStore := NewStore()
	if err := newStore.OpenDatabases([]string{tempDir}); err != nil {
		t.Fatal(err)
	}

	// original DB should now be open
	commands, err := newStore.QueryCommands(ctx)
	if err != nil {
		t.Fatal(err)
	}
	require.Len(t, commands, 1)
	c2 := commands[0]
	require.Equal(t, c.Borg, c2.Borg)
	require.Equal(t, c.Command, c2.Command)
}
