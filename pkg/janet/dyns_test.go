package janet

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestDyns(t *testing.T) {
	ctx := context.Background()
	vm, err := New(ctx)
	require.NoError(t, err)

	t.Run("yield dyn value", func(t *testing.T) {
		call := CallString(`(yield (dyn :my-value))`)
		call.Options.Dyns = map[Keyword]any{
			Keyword("my-value"): 42,
		}

		result, err := vm.ExecuteCall(ctx, nil, call)
		require.NoError(t, err)
		require.NotNil(t, result.Yield)

		var got int
		err = result.Yield.Unmarshal(&got)
		require.NoError(t, err)
		require.Equal(t, 42, got)
	})

	t.Run("multiple dyns", func(t *testing.T) {
		call := CallString(`(yield (+ (dyn :a) (dyn :b)))`)
		call.Options.Dyns = map[Keyword]any{
			Keyword("a"): 10,
			Keyword("b"): 5,
		}

		result, err := vm.ExecuteCall(ctx, nil, call)
		require.NoError(t, err)
		require.NotNil(t, result.Yield)

		var got int
		err = result.Yield.Unmarshal(&got)
		require.NoError(t, err)
		require.Equal(t, 15, got)
	})
}
