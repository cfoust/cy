package protocol

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSerialization(t *testing.T) {
	before := SizeMessage{
		Width:  80,
		Height: 26,
	}

	encoded, err := Encode(before)
	assert.NoError(t, err)

	after, err := Decode(encoded)
	assert.Equal(t, &before, after, "should yield same result")
}
