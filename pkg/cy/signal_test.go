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

	done := make(chan error, 1)
	go func() {
		done <- reg.WaitFor(
			context.Background(),
			"test",
			5,
		)
	}()

	// Give the waiter time to register
	time.Sleep(10 * time.Millisecond)
	reg.Signal("test")

	select {
	case err := <-done:
		require.NoError(t, err)
	case <-time.After(2 * time.Second):
		t.Fatal("WaitFor did not return after Signal")
	}
}

func TestSignalMultipleWaiters(t *testing.T) {
	reg := newSignalRegistry()

	var wg sync.WaitGroup
	errs := make([]error, 3)

	for i := range 3 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			errs[i] = reg.WaitFor(
				context.Background(),
				"multi",
				5,
			)
		}()
	}

	time.Sleep(10 * time.Millisecond)
	reg.Signal("multi")
	wg.Wait()

	for i, err := range errs {
		require.NoError(t, err, "waiter %d got error", i)
	}
}

func TestSignalTimeout(t *testing.T) {
	reg := newSignalRegistry()

	err := reg.WaitFor(
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

	done := make(chan error, 1)
	go func() {
		done <- reg.WaitFor(ctx, "canceled", 0)
	}()

	time.Sleep(10 * time.Millisecond)
	cancel()

	select {
	case err := <-done:
		require.ErrorIs(t, err, context.Canceled)
	case <-time.After(2 * time.Second):
		t.Fatal("WaitFor did not return after cancel")
	}
}

func TestSignalSendBeforeWait(t *testing.T) {
	reg := newSignalRegistry()

	// Signal with no waiters should not panic or error
	reg.Signal("nobody")

	// A subsequent wait should still block (signal was lost)
	err := reg.WaitFor(
		context.Background(),
		"nobody",
		0.05,
	)
	require.Error(t, err)
	require.Contains(t, err.Error(), "timed out")
}

func TestSignalIndependentChannels(t *testing.T) {
	reg := newSignalRegistry()

	done := make(chan error, 1)
	go func() {
		done <- reg.WaitFor(
			context.Background(),
			"channel-a",
			1,
		)
	}()

	time.Sleep(10 * time.Millisecond)

	// Signaling a different channel should not wake the waiter
	reg.Signal("channel-b")

	select {
	case <-done:
		t.Fatal("waiter on channel-a woke from signal to channel-b")
	case <-time.After(50 * time.Millisecond):
		// Good, still waiting
	}

	// Now signal the correct channel
	reg.Signal("channel-a")

	select {
	case err := <-done:
		require.NoError(t, err)
	case <-time.After(2 * time.Second):
		t.Fatal("WaitFor did not return after correct Signal")
	}
}
