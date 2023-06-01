package protocol

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSerialization(t *testing.T) {
	before := SizeMessage{
		Rows:    80,
		Columns: 26,
	}

	encoded, err := Encode(before)
	assert.NoError(t, err)

	after, err := Decode(encoded)
	assert.Equal(t, &before, after, "should yield same result")
}
