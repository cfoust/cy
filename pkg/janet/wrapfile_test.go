package janet

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestWrapFile(t *testing.T) {
	ctx := context.Background()
	vm, err := New(ctx)
	require.NoError(t, err)

	t.Run("wrap for reading", func(t *testing.T) {
		tmpDir := t.TempDir()
		tmpFile := filepath.Join(tmpDir, "test.txt")
		err := os.WriteFile(tmpFile, []byte("hello"), 0644)
		require.NoError(t, err)

		file, err := os.Open(tmpFile)
		require.NoError(t, err)
		defer func() { _ = file.Close() }()

		wrapped, err := vm.WrapFile(ctx, file, FileFlagRead)
		require.NoError(t, err)
		require.NotNil(t, wrapped)
		defer wrapped.Free()
	})

	t.Run("wrap for writing", func(t *testing.T) {
		tmpDir := t.TempDir()
		tmpFile := filepath.Join(tmpDir, "out.txt")

		file, err := os.Create(tmpFile)
		require.NoError(t, err)
		defer func() { _ = file.Close() }()

		wrapped, err := vm.WrapFile(ctx, file, FileFlagWrite)
		require.NoError(t, err)
		require.NotNil(t, wrapped)
		defer wrapped.Free()
	})
}

func TestPrint(t *testing.T) {
	ctx := context.Background()
	vm, err := New(ctx)
	require.NoError(t, err)

	t.Run("print to :out", func(t *testing.T) {
		tmpDir := t.TempDir()
		outFile := filepath.Join(tmpDir, "stdout.txt")

		file, err := os.Create(outFile)
		require.NoError(t, err)
		defer func() { _ = file.Close() }()

		wrapped, err := vm.WrapFile(ctx, file, FileFlagWrite)
		require.NoError(t, err)
		require.NotNil(t, wrapped)
		defer wrapped.Free()

		call := CallString(`(print "hello from print") (flush)`)
		call.Options.Dyns = map[Keyword]any{
			Keyword("out"): wrapped,
		}

		result, err := vm.ExecuteCall(ctx, nil, call)
		require.NoError(t, err)
		require.NotNil(t, result)

		_ = file.Close()
		content, err := os.ReadFile(outFile)
		require.NoError(t, err)
		require.Equal(t, "hello from print\n", string(content))
	})

	t.Run("eprint to :err", func(t *testing.T) {
		tmpDir := t.TempDir()
		errFile := filepath.Join(tmpDir, "stderr.txt")

		file, err := os.Create(errFile)
		require.NoError(t, err)
		defer func() { _ = file.Close() }()

		wrapped, err := vm.WrapFile(ctx, file, FileFlagWrite)
		require.NoError(t, err)
		require.NotNil(t, wrapped)
		defer wrapped.Free()

		call := CallString(`(eprint "error from eprint") (eflush)`)
		call.Options.Dyns = map[Keyword]any{
			Keyword("err"): wrapped,
		}

		result, err := vm.ExecuteCall(ctx, nil, call)
		require.NoError(t, err)
		require.NotNil(t, result)

		_ = file.Close()
		content, err := os.ReadFile(errFile)
		require.NoError(t, err)
		require.Equal(t, "error from eprint\n", string(content))
	})
}
