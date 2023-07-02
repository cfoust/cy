package janet

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCallback(t *testing.T) {
	vm, err := New(context.Background())
	assert.NoError(t, err)

	ok := false
	err = vm.Callback("test", func() {
		ok = true
	})
	assert.NoError(t, err)

	err = vm.Execute(`(go/exec "test")`)
	assert.NoError(t, err)

	assert.True(t, ok, "should have been called")
}
