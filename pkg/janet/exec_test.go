package janet

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestExecuteFunction(t *testing.T) {
	ctx := context.Background()
	vm, err := New(ctx)
	require.NoError(t, err)

	t.Run("basic function call", func(t *testing.T) {
		err := vm.Execute(ctx, `(defn test-add [a b] (+ a b))`)
		require.NoError(t, err)

		err = vm.ExecuteFunction(ctx, nil, "test-add", 2, 3)
		require.NoError(t, err)
	})

	t.Run("non-existent function", func(t *testing.T) {
		err := vm.ExecuteFunction(ctx, nil, "non-existent-function")
		require.Error(t, err)
	})

	t.Run("function with different parameter types", func(t *testing.T) {
		err := vm.Execute(ctx, `(defn test-params [num str bool]
			(print "num:" num "str:" str "bool:" bool))`)
		require.NoError(t, err)
		err = vm.ExecuteFunction(ctx, nil, "test-params", 42, "hello", true)
		require.NoError(t, err)
	})

	t.Run("function with no parameters", func(t *testing.T) {
		err := vm.Execute(ctx, `(defn test-no-params [] (print "no params"))`)
		require.NoError(t, err)

		err = vm.ExecuteFunction(ctx, nil, "test-no-params")
		require.NoError(t, err)
	})

	t.Run("function that errors", func(t *testing.T) {
		err := vm.Execute(ctx, `(defn test-error [] (error "test error"))`)
		require.NoError(t, err)

		err = vm.ExecuteFunction(ctx, nil, "test-error")
		require.Error(t, err)
		require.Contains(t, err.Error(), "test error")
	})

	t.Run("function with user context", func(t *testing.T) {
		var receivedUser interface{}
		err := vm.Callback(
			"test-user-context",
			"",
			func(ctx context.Context, user interface{}) {
				receivedUser = user
			},
		)
		require.NoError(t, err)
		err = vm.Execute(ctx, `(defn test-with-user [] (test-user-context))`)
		require.NoError(t, err)
		testUser := "test-user"
		err = vm.ExecuteFunction(ctx, testUser, "test-with-user")
		require.NoError(t, err)
		require.Equal(t, testUser, receivedUser)
	})

	t.Run("variable function", func(t *testing.T) {
		err := vm.Execute(
			ctx,
			`(def test-var-fn (fn [] (print "variable function")))`,
		)
		require.NoError(t, err)

		err = vm.ExecuteFunction(ctx, nil, "test-var-fn")
		require.NoError(t, err)
	})

	t.Run("function with complex return", func(t *testing.T) {
		err := vm.Execute(
			ctx,
			`(defn test-complex-return [] {:a 1 :b "hello" :c [1 2 3]})`,
		)
		require.NoError(t, err)

		err = vm.ExecuteFunction(ctx, nil, "test-complex-return")
		require.NoError(t, err)
	})

	t.Run("built-in function", func(t *testing.T) {
		err = vm.ExecuteFunction(ctx, nil, "length", []int{1, 2, 3, 4})
		require.NoError(t, err)
	})

	t.Run("wrong parameter count", func(t *testing.T) {
		err := vm.Execute(ctx, `(defn test-param-count [a b] (+ a b))`)
		require.NoError(t, err)
		err = vm.ExecuteFunction(ctx, nil, "test-param-count", 1)
		require.Error(t, err)
	})

	t.Run("context cancellation", func(t *testing.T) {
		err := vm.Execute(ctx, `(defn test-slow [] (print "slow function"))`)
		require.NoError(t, err)

		cancelledCtx, cancel := context.WithCancel(ctx)
		cancel()

		err = vm.ExecuteFunction(cancelledCtx, nil, "test-slow")
		require.Error(t, err)
		require.Contains(t, err.Error(), "context canceled")
	})
}
