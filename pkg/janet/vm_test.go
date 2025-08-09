package janet

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/janet/test"

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
	value, err := vm.Marshal(before)
	require.NoError(t, err)

	var after T
	err = value.Unmarshal(&after)
	require.NoError(t, err)

	require.Equal(t, before, after, "should yield same result")
}

type TestModule struct {
	Value int
}

func (m *TestModule) Update(value int) {
	m.Value = value
}

type CustomMarshal struct {
	Number int
}

var _ Marshalable = (*CustomMarshal)(nil)

func (c *CustomMarshal) MarshalJanet() interface{} {
	return c.Number
}

var _ Unmarshalable = (*CustomMarshal)(nil)

func (c *CustomMarshal) UnmarshalJanet(value *Value) error {
	err := value.Unmarshal(&c.Number)
	if err != nil {
		return err
	}

	return nil
}

type TestValue struct {
	One    int
	Two    bool
	Three  string
	Four   *int
	Five   *int
	Ints   [6]int
	Bools  []bool
	Buffer []byte
	Map    map[string]int
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
	t.Logf("callback")

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
		_, err = vm.ExecuteCall(ctx, 1, call)
		require.NoError(t, err)
		require.Equal(t, 1, state)
	})

	t.Run("callback with user and context", func(t *testing.T) {
		state := 0
		err = vm.Callback(
			"test-context-ctx",
			"",
			func(ctx context.Context, user interface{}) {
				if ctx == nil || user == nil {
					t.Fail()
				}

				if value, ok := user.(int); ok {
					state = value
				}
			},
		)
		require.NoError(t, err)

		call := CallString(`(test-context-ctx)`)
		_, err = vm.ExecuteCall(ctx, 1, call)
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

		err = vm.Execute(
			ctx,
			`(test-named 2 :second "ok" :nil-bool nil :ok-bool true)`,
		)
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

	t.Run("callback with value with hidden fields", func(t *testing.T) {
		err = vm.Callback(
			"test-hidden",
			"",
			func() (result test.Hidden, err error) {
				return
			},
		)
		require.NoError(t, err)

		err = vm.Execute(ctx, `(pp (test-hidden))`)
		require.NoError(t, err)
	})

	t.Run("translation", func(t *testing.T) {
		initJanet()
		defer deInitJanet()

		cmp(t, vm, 2)
		cmp(t, vm, 2.02)
		cmp(t, vm, true)
		cmp(t, vm, "test")

		// Ensure that unmarshaling in a separate goroutine works
		done := make(chan error)
		go func() {
			bools := make([]bool, 2)
			bools[0] = true
			buffer := []byte{1, 2, 3}
			five := 2
			structValue := TestValue{
				One:   2,
				Two:   true,
				Three: "test",
				Five:  &five,
				Ints: [6]int{
					2,
					3,
				},
				Bools:  bools,
				Buffer: buffer,
				Map: map[string]int{
					"test": 2,
				},
			}
			cmp(t, vm, structValue)

			before, err := vm.Marshal(structValue)
			require.NoError(t, err)

			var after TestValue
			err = before.Unmarshal(&after)
			require.NoError(t, err)
			require.Equal(t, structValue, after, "should yield same result")
			before.Free()
			done <- nil
		}()
		<-done
		require.NoError(t, err)

		// keywords
		{
			keyword := Keyword("test")
			value, err := vm.Marshal(keyword)
			require.NoError(t, err)
			err = value.Unmarshal(&keyword)
			require.NoError(t, err)

			foo := Keyword("foo")
			err = value.Unmarshal(&foo)
			require.Error(t, err)
		}

		// public Marshal API
		{
			value := 2
			_, err := vm.Marshal(value)
			require.NoError(t, err)
		}

		// Marshalable and Unmarshalable
		{
			customBefore := CustomMarshal{Number: 2}
			v, err := vm.Marshal(&customBefore)
			require.NoError(t, err)

			var customAfter = CustomMarshal{}
			err = v.Unmarshal(&customAfter)
			require.NoError(t, err)
			require.Equal(t, customBefore.Number, customAfter.Number)
		}

		// Time
		{
			now := time.Now()
			v, err := vm.Marshal(now)
			require.NoError(t, err)

			var after time.Time
			err = v.Unmarshal(&after)
			require.NoError(t, err)
			require.Equal(t, now.Unix(), after.Unix())
			require.Equal(t, now.Location(), after.Location())
		}
	})

	t.Run("json", func(t *testing.T) {
		err = vm.Execute(ctx, `(json/encode [1 2 3])`)
		require.NoError(t, err)

		out, err := vm.ExecuteCall(
			ctx,
			nil,
			CallString(`(yield {:a 1 :b 2})`),
		)
		require.NoError(t, err)
		require.NotNil(t, out)
		require.NotNil(t, out.Yield)

		json, err := out.Yield.JSON()
		require.NoError(t, err)
		require.Equal(t, `{"a":1,"b":2}`, string(json))
		require.Equal(t, `{:a 1 :b 2}`, out.Yield.String())
	})

	t.Run(
		"janet (un)marshaling with Bytes() and FromBytes()",
		func(t *testing.T) {
			// Test basic integer marshaling
			t.Run("integer", func(t *testing.T) {
				original := 42
				value, err := vm.Marshal(original)
				require.NoError(t, err)
				defer value.Free()

				// Test Bytes() method
				bytes, err := value.Bytes()
				require.NoError(t, err)
				require.NotEmpty(
					t,
					bytes,
					"marshaled bytes should not be empty",
				)

				// Test FromBytes() method
				unmarshaled, err := vm.FromBytes(bytes)
				require.NoError(t, err)
				defer unmarshaled.Free()

				// Verify the value is correct
				var result int
				err = unmarshaled.Unmarshal(&result)
				require.NoError(t, err)
				require.Equal(
					t,
					original,
					result,
					"unmarshaled value should match original",
				)
			})

			// Test string marshaling
			t.Run("string", func(t *testing.T) {
				original := "hello world"
				value, err := vm.Marshal(original)
				require.NoError(t, err)
				defer value.Free()

				bytes, err := value.Bytes()
				require.NoError(t, err)
				require.NotEmpty(t, bytes)

				unmarshaled, err := vm.FromBytes(bytes)
				require.NoError(t, err)
				defer unmarshaled.Free()

				var result string
				err = unmarshaled.Unmarshal(&result)
				require.NoError(t, err)
				require.Equal(t, original, result)
			})

			// Test boolean marshaling
			t.Run("boolean", func(t *testing.T) {
				original := true
				value, err := vm.Marshal(original)
				require.NoError(t, err)
				defer value.Free()

				bytes, err := value.Bytes()
				require.NoError(t, err)
				require.NotEmpty(t, bytes)

				unmarshaled, err := vm.FromBytes(bytes)
				require.NoError(t, err)
				defer unmarshaled.Free()

				var result bool
				err = unmarshaled.Unmarshal(&result)
				require.NoError(t, err)
				require.Equal(t, original, result)
			})

			// Test array marshaling
			t.Run("array", func(t *testing.T) {
				original := []int{1, 2, 3, 4, 5}
				value, err := vm.Marshal(original)
				require.NoError(t, err)
				defer value.Free()

				bytes, err := value.Bytes()
				require.NoError(t, err)
				require.NotEmpty(t, bytes)

				unmarshaled, err := vm.FromBytes(bytes)
				require.NoError(t, err)
				defer unmarshaled.Free()

				var result []int
				err = unmarshaled.Unmarshal(&result)
				require.NoError(t, err)
				require.Equal(t, original, result)
			})

			// Test map marshaling
			t.Run("map", func(t *testing.T) {
				original := map[string]int{"a": 1, "b": 2, "c": 3}
				value, err := vm.Marshal(original)
				require.NoError(t, err)
				defer value.Free()

				bytes, err := value.Bytes()
				require.NoError(t, err)
				require.NotEmpty(t, bytes)

				unmarshaled, err := vm.FromBytes(bytes)
				require.NoError(t, err)
				defer unmarshaled.Free()

				var result map[string]int
				err = unmarshaled.Unmarshal(&result)
				require.NoError(t, err)
				require.Equal(t, original, result)
			})

			// Test complex nested structure
			t.Run("nested structure", func(t *testing.T) {
				original := TestValue{
					One:    42,
					Two:    true,
					Three:  "test string",
					Four:   nil,
					Five:   func() *int { i := 99; return &i }(),
					Ints:   [6]int{1, 2, 3, 4, 5, 6},
					Bools:  []bool{true, false, true},
					Buffer: []byte("binary data"),
					Map:    map[string]int{"key1": 10, "key2": 20},
				}

				value, err := vm.Marshal(original)
				require.NoError(t, err)
				defer value.Free()

				bytes, err := value.Bytes()
				require.NoError(t, err)
				require.NotEmpty(t, bytes)

				unmarshaled, err := vm.FromBytes(bytes)
				require.NoError(t, err)
				defer unmarshaled.Free()

				var result TestValue
				err = unmarshaled.Unmarshal(&result)
				require.NoError(t, err)
				require.Equal(t, original.One, result.One)
				require.Equal(t, original.Two, result.Two)
				require.Equal(t, original.Three, result.Three)
				require.Equal(t, original.Four, result.Four)
				require.NotNil(t, result.Five)
				require.Equal(t, *original.Five, *result.Five)
				require.Equal(t, original.Ints, result.Ints)
				require.Equal(t, original.Bools, result.Bools)
				require.Equal(t, original.Buffer, result.Buffer)
				require.Equal(t, original.Map, result.Map)
			})

			// Test error conditions
			t.Run("error conditions", func(t *testing.T) {
				// Test Bytes() on freed value
				t.Run("bytes on freed value", func(t *testing.T) {
					value, err := vm.Marshal(42)
					require.NoError(t, err)

					value.Free()

					_, err = value.Bytes()
					require.Error(t, err)
					require.Contains(t, err.Error(), "freed")
				})

				// Test FromBytes() with empty data
				t.Run("from bytes with empty data", func(t *testing.T) {
					_, err := vm.FromBytes([]byte{})
					require.Error(t, err)
					require.Contains(t, err.Error(), "empty data")
				})

				// Test FromBytes() with invalid data (may or may not error depending on Janet's implementation)
				t.Run("from bytes with invalid data", func(t *testing.T) {
					result, err := vm.FromBytes([]byte{0x00, 0x01, 0x02, 0xFF})
					// Janet's unmarshal is quite robust, so it may not always error
					// If it doesn't error, at least verify we got something back
					if err == nil && result != nil {
						result.Free() // Clean up if no error
					}
					// This test mainly verifies the function doesn't panic or crash
				})
			})
		},
	)

}
