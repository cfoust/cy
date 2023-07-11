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

func cmp[T any](t *testing.T, vm *VM, before T) {
	value, err := vm.marshal(before)
	assert.NoError(t, err)

	var after T
	err = vm.unmarshal(value, &after)
	assert.NoError(t, err)

	t.Logf("%+v", after)
	assert.Equal(t, before, after, "should yield same result")
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

	t.Run("try a callback with a function", func(t *testing.T) {
		var fun *Function
		err = vm.Callback("test-callback", func(f *Function) {
			fun = f
		})
		assert.NoError(t, err)

		err = vm.Execute(`(test-callback (fn [first second &] (+ 2 2)))`)
		assert.NoError(t, err)
		assert.NotNil(t, fun)

		err = fun.Call("2312", 2)
		assert.NoError(t, err)
	})

	t.Run("try a callback with context", func(t *testing.T) {
		state := 0
		err = vm.Callback("test-context", func(context interface{}) {
			if value, ok := context.(int); ok {
				state = value
			}
		})
		assert.NoError(t, err)

		call := CallString(`(test-context)`)
		call.Context = 1
		err = vm.ExecuteCall(call)
		assert.NoError(t, err)
		assert.Equal(t, 1, state)
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

	t.Run("defining a symbol", func(t *testing.T) {
		err = vm.Execute(`(def some-int 2)`)
		assert.NoError(t, err)

		err = vm.Execute(`(+ some-int some-int)`)
		assert.NoError(t, err)
	})

	t.Run("translation", func(t *testing.T) {
		initJanet()
		defer deInitJanet()

		cmp(t, vm, 2)
		cmp(t, vm, 2.02)
		cmp(t, vm, true)
		cmp(t, vm, "test")

		type Value struct {
			One   int
			Two   bool
			Three string
			Ints  [6]int
			Bools []bool
		}

		bools := make([]bool, 2)
		bools[0] = true
		cmp(t, vm, Value{
			One:   2,
			Two:   true,
			Three: "test",
			Ints: [6]int{
				2,
				3,
			},
			Bools: bools,
		})
	})
}
