package janet

/*
#cgo CFLAGS: -std=c99
#cgo LDFLAGS: -lm -ldl

#include <stdlib.h>
#include <janet.h>
#include <api.h>
*/
import "C"

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
)

func cmp[T any](t *testing.T, before T) {
	value, err := marshal(before)
	assert.NoError(t, err)

	var after T
	err = unmarshal(value, &after)
	assert.NoError(t, err)

	t.Logf("%+v", after)
	assert.Equal(t, before, after, "should yield same result")
}

func TestPrimitive(t *testing.T) {
	cmp(t, 2)
	cmp(t, 2.02)
	cmp(t, true)
	cmp(t, "test")
}

func TestMain(m *testing.M) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	vm, _ := New(ctx)
	vm.Execute("")
	m.Run()
}
