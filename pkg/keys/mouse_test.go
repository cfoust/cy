package keys

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func testMouseInput(t *testing.T, input string) {
	bytes := []byte(input)
	msg, _ := Read(bytes)
	mouse, ok := msg.(Mouse)
	if !ok {
		t.Fail()
	}
	assert.Equal(t, bytes, mouse.Bytes())
}

func TestMouse(t *testing.T) {
	testMouseInput(t, "\u001b[M :;")
	testMouseInput(t, "\u001b[M@;;")
	testMouseInput(t, "\u001b[M`9>")
	testMouseInput(t, "\u001b[MaD3")
	testMouseInput(t, "\u001b[Mc>;")
	testMouseInput(t, "\u001b[Mc74")
	testMouseInput(t, "\u001b[MC}2")
	testMouseInput(t, "\u001b[M@P6")
	testMouseInput(t, "\u001b[M#Q6")
	testMouseInput(t, "\u001b[MCu,")
	testMouseInput(t, "\u001b[MbM<")
}
