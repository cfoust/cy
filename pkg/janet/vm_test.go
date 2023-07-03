package janet

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
)

func writeFile(path string, data []byte) error {
	f, err := os.Create(path)
	if err != nil {
		return err
	}

	_, err = f.Write(data)
	return err
}

func TestVM(t *testing.T) {
	// TODO(cfoust): 07/02/23 gracefully handle the Janet vm already being
	// initialized and break these into separate tests
	vm, err := New(context.Background())
	assert.NoError(t, err)

	ok := false
	err = vm.Callback("test", func() {
		ok = true
	})
	assert.NoError(t, err)

	t.Run("try a callback", func(t *testing.T) {
		ok = false
		err = vm.Execute(`(test)`)
		assert.NoError(t, err)

		assert.True(t, ok, "should have been called")
	})

	t.Run("execute a file", func(t *testing.T) {
		ok = false

		filename := filepath.Join(t.TempDir(), "test.janet")
		err := writeFile(
			filename,
			[]byte(`(test)`),
		)
		assert.NoError(t, err)

		err = vm.ExecuteFile(filename)
		assert.NoError(t, err)

		assert.True(t, ok, "should have been called")
	})

	t.Run("catches a syntax error", func(t *testing.T) {
		err = vm.Execute(`(asd`)
		assert.Error(t, err)
	})
}
