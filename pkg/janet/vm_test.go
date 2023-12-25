package janet

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/require"
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
	require.NoError(t, err)

	var after T
	err = vm.unmarshal(value, &after)
	require.NoError(t, err)

	require.Equal(t, before, after, "should yield same result")
}

type TestModule struct {
	Value int
}

func (m *TestModule) Update(value int) {
	m.Value = value
}

func TestVM(t *testing.T) {
	ctx := context.Background()
	// TODO(cfoust): 07/02/23 gracefully handle the Janet vm already being
	// initialized and break these into separate tests
	vm, err := New(ctx)
	require.NoError(t, err)

	ok := false
	err = vm.Callback("test", "", func() {
		ok = true
	})
	require.NoError(t, err)

	t.Run("callback", func(t *testing.T) {
		ok = false
		err = vm.Execute(ctx, `(test)`)
		require.NoError(t, err)

		require.True(t, ok, "should have been called")
	})

	t.Run("callback with a function", func(t *testing.T) {
		var fun *Function
		err = vm.Callback("test-callback", "", func(f *Function) {
			fun = f
		})
		require.NoError(t, err)

		err = vm.Execute(ctx, `(test-callback (fn [first second &] (+ 2 2)))`)
		require.NoError(t, err)
		require.NotNil(t, fun)

		err = fun.Call(ctx, "2312", 2)
		require.NoError(t, err)
	})

	t.Run("callback with context", func(t *testing.T) {
		state := 0
		err = vm.Callback("test-context", "", func(context interface{}) {
			if value, ok := context.(int); ok {
				state = value
			}
		})
		require.NoError(t, err)

		call := CallString(`(test-context)`)
		err = vm.ExecuteCall(ctx, 1, call)
		require.NoError(t, err)
		require.Equal(t, 1, state)
	})

	t.Run("callback with user and context", func(t *testing.T) {
		state := 0
		err = vm.Callback("test-context-ctx", "", func(ctx context.Context, user interface{}) {
			if ctx == nil || user == nil {
				t.Fail()
			}

			if value, ok := user.(int); ok {
				state = value
			}
		})
		require.NoError(t, err)

		call := CallString(`(test-context-ctx)`)
		err = vm.ExecuteCall(ctx, 1, call)
		require.NoError(t, err)
		require.Equal(t, 1, state)
	})

	t.Run("callback with nil return", func(t *testing.T) {
		value := 0
		err = vm.Callback("test-nil", "", func(param *int) *int {
			if param == nil {
				value = 1
				return nil
			}

			value = 2
			return param
		})
		require.NoError(t, err)

		err = vm.Execute(ctx, `(test-nil nil)`)
		require.NoError(t, err)
		require.Equal(t, 1, value)

		err = vm.Execute(ctx, `(test-nil 2)`)
		require.NoError(t, err)
		require.Equal(t, 2, value)
	})

	t.Run("callback with named arguments", func(t *testing.T) {
		type Params struct {
			First   int
			Second  string
			NilBool *bool
			OkBool  *bool
		}

		var post Params
		err = vm.Callback("test-named", "", func(
			context interface{},
			value int,
			params *Named[Params],
		) {
			post = params.WithDefault(Params{
				First: 2,
			})
		})
		require.NoError(t, err)

		err = vm.Execute(ctx, `(test-named 2 :second "ok" :nil-bool nil :ok-bool true)`)
		require.NoError(t, err)

		require.Equal(t, 2, post.First)
		require.Equal(t, true, *post.OkBool)
		require.Equal(t, (*bool)(nil), post.NilBool)
		require.Equal(t, "ok", post.Second)
	})

	t.Run("callback with value", func(t *testing.T) {
		first := 0
		err = vm.Callback("test-value", "", func(value *Value) error {
			defer value.Free()

			var nums []int
			err := value.Unmarshal(&nums)
			if err != nil {
				return err
			}

			first = nums[0]

			return nil
		})
		require.NoError(t, err)

		err = vm.Execute(ctx, `(test-value [1 2 3])`)
		require.NoError(t, err)

		require.Equal(t, 1, first)
	})

	t.Run("callback with a tuple", func(t *testing.T) {
		type Tuple struct {
			_      struct{} `janet:"tuple"`
			First  int
			Second string
		}

		first := 0
		err = vm.Callback("test-tuple", "", func(t Tuple) {
			first = t.First
		})
		require.NoError(t, err)

		err = vm.Execute(ctx, `(test-tuple [1 "two"])`)
		require.NoError(t, err)
		require.Equal(t, 1, first)

		err = vm.Execute(ctx, `(test-tuple [1 2 3])`)
		require.Error(t, err)

		err = vm.Execute(ctx, `(test-tuple ["one" "two"])`)
		require.Error(t, err)
	})

	t.Run("module registration", func(t *testing.T) {
		m := &TestModule{}
		err = vm.Module("test", m)
		require.NoError(t, err)

		err = vm.Execute(ctx, `(test/update 1)`)
		require.NoError(t, err)
		require.Equal(t, 1, m.Value)
	})

	t.Run("execute a file", func(t *testing.T) {
		ok = false

		filename := filepath.Join(t.TempDir(), "test.janet")
		err := writeFile(
			filename,
			[]byte(`(test)`),
		)
		require.NoError(t, err)

		err = vm.ExecuteFile(ctx, filename)
		require.NoError(t, err)

		require.True(t, ok, "should have been called")
	})

	t.Run("catches a syntax error", func(t *testing.T) {
		err = vm.Execute(ctx, `(asd`)
		require.Error(t, err)
	})

	t.Run("defining a symbol", func(t *testing.T) {
		err = vm.Execute(ctx, `(def some-int 2)`)
		require.NoError(t, err)

		err = vm.Execute(ctx, `(+ some-int some-int)`)
		require.NoError(t, err)
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

		// keywords
		keyword := Keyword("test")
		value, err := vm.marshal(keyword)
		require.NoError(t, err)
		err = vm.unmarshal(value, &keyword)
		require.NoError(t, err)
		foo := Keyword("foo")
		err = vm.unmarshal(value, &foo)
		require.Error(t, err)
	})
}
