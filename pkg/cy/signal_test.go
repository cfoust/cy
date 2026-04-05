package cy

import (
	"context"
	"sync"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

func TestSignalBasic(t *testing.T) {
	reg := newSignalRegistry()

	type result struct {
		value interface{}
		err   error
	}
	done := make(chan result, 1)
	go func() {
		v, err := reg.WaitFor(
			context.Background(),
			"test",
			5,
		)
		done <- result{v, err}
	}()

	// Give the waiter time to register
	time.Sleep(10 * time.Millisecond)
	reg.Signal("test", "hello")

	select {
	case r := <-done:
		require.NoError(t, r.err)
		require.Equal(t, "hello", r.value)
	case <-time.After(2 * time.Second):
		t.Fatal("WaitFor did not return after Signal")
	}
}

func TestSignalMultipleWaiters(t *testing.T) {
	reg := newSignalRegistry()

	var mu sync.Mutex
	var wg sync.WaitGroup
	values := make([]interface{}, 3)
	errs := make([]error, 3)

	for i := range 3 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			v, err := reg.WaitFor(
				context.Background(),
				"multi",
				5,
			)
			mu.Lock()
			values[i] = v
			errs[i] = err
			mu.Unlock()
		}()
	}

	time.Sleep(10 * time.Millisecond)
	reg.Signal("multi", 42)
	wg.Wait()

	for i := range 3 {
		require.NoError(t, errs[i], "waiter %d got error", i)
		require.Equal(t, 42, values[i], "waiter %d got wrong value", i)
	}
}

func TestSignalNilValue(t *testing.T) {
	reg := newSignalRegistry()

	type result struct {
		value interface{}
		err   error
	}
	done := make(chan result, 1)
	go func() {
		v, err := reg.WaitFor(
			context.Background(),
			"nil-test",
			5,
		)
		done <- result{v, err}
	}()

	time.Sleep(10 * time.Millisecond)
	reg.Signal("nil-test", nil)

	select {
	case r := <-done:
		require.NoError(t, r.err)
		require.Nil(t, r.value)
	case <-time.After(2 * time.Second):
		t.Fatal("WaitFor did not return after Signal")
	}
}

func TestSignalTimeout(t *testing.T) {
	reg := newSignalRegistry()

	_, err := reg.WaitFor(
		context.Background(),
		"never",
		0.05,
	)
	require.Error(t, err)
	require.Contains(t, err.Error(), "timed out")

	// Verify the channel was cleaned up
	reg.Lock()
	_, exists := reg.channels["never"]
	reg.Unlock()
	require.False(t, exists, "channel should be removed after timeout")
}

func TestSignalContextCancel(t *testing.T) {
	reg := newSignalRegistry()

	ctx, cancel := context.WithCancel(context.Background())

	type result struct {
		value interface{}
		err   error
	}
	done := make(chan result, 1)
	go func() {
		v, err := reg.WaitFor(ctx, "canceled", 0)
		done <- result{v, err}
	}()

	time.Sleep(10 * time.Millisecond)
	cancel()

	select {
	case r := <-done:
		require.ErrorIs(t, r.err, context.Canceled)
	case <-time.After(2 * time.Second):
		t.Fatal("WaitFor did not return after cancel")
	}
}

func TestSignalSendBeforeWait(t *testing.T) {
	reg := newSignalRegistry()

	// Signal with no waiters should not panic or error
	reg.Signal("nobody", "lost")

	// A subsequent wait should still block (signal was lost)
	_, err := reg.WaitFor(
		context.Background(),
		"nobody",
		0.05,
	)
	require.Error(t, err)
	require.Contains(t, err.Error(), "timed out")
}

func TestSignalIndependentChannels(t *testing.T) {
	reg := newSignalRegistry()

	type result struct {
		value interface{}
		err   error
	}
	done := make(chan result, 1)
	go func() {
		v, err := reg.WaitFor(
			context.Background(),
			"channel-a",
			1,
		)
		done <- result{v, err}
	}()

	time.Sleep(10 * time.Millisecond)

	// Signaling a different channel should not wake the waiter
	reg.Signal("channel-b", "wrong")

	select {
	case <-done:
		t.Fatal("waiter on channel-a woke from signal to channel-b")
	case <-time.After(50 * time.Millisecond):
		// Good, still waiting
	}

	// Now signal the correct channel
	reg.Signal("channel-a", "right")

	select {
	case r := <-done:
		require.NoError(t, r.err)
		require.Equal(t, "right", r.value)
	case <-time.After(2 * time.Second):
		t.Fatal("WaitFor did not return after correct Signal")
	}
}
