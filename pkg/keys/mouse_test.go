package keys

import (
	"fmt"
	"testing"

	"github.com/cfoust/cy/pkg/emu"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func testMouseInput(t *testing.T, input string) {
	b := []byte(input)
	msg, _ := Read(b)
	mouse, ok := msg.(Mouse)
	if !ok {
		t.Fail()
	}
	data, ok := mouse.Bytes(emu.ModeMouseMany)
	assert.True(t, ok)
	assert.Equal(t, b, data)
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

// testSGRMouseInput verifies that an SGR-encoded mouse sequence
// round-trips through Read and Bytes with ModeMouseSgr.
func testSGRMouseInput(t *testing.T, input string) {
	t.Helper()
	b := []byte(input)
	msg, w := Read(b)
	mouse, ok := msg.(Mouse)
	require.True(t, ok, "expected Mouse, got %T for %q", msg, input)
	assert.Equal(t, len(b), w, "consumed width")
	data, ok := mouse.Bytes(emu.ModeMouseMany | emu.ModeMouseSgr)
	assert.True(t, ok)
	assert.Equal(t, b, data)
}

func TestSGRMouse(t *testing.T) {
	// Left press at (0,0)
	testSGRMouseInput(t, "\x1b[<0;1;1M")
	// Left release at (0,0)
	testSGRMouseInput(t, "\x1b[<0;1;1m")
	// Right press at (10,20)
	testSGRMouseInput(t, "\x1b[<2;11;21M")
	// Middle press at (5,5)
	testSGRMouseInput(t, "\x1b[<1;6;6M")
	// Wheel up
	testSGRMouseInput(t, "\x1b[<64;10;10M")
	// Wheel down
	testSGRMouseInput(t, "\x1b[<65;10;10M")
	// Motion with left button (drag)
	testSGRMouseInput(t, "\x1b[<32;15;25M")
	// Motion without button
	testSGRMouseInput(t, "\x1b[<35;15;25M")
	// Ctrl+left click
	testSGRMouseInput(t, "\x1b[<16;5;5M")
	// Alt+left click
	testSGRMouseInput(t, "\x1b[<8;5;5M")
	// Large coordinates (beyond X10 range)
	testSGRMouseInput(t, "\x1b[<0;300;200M")
}

func TestMouseFiltering(t *testing.T) {
	press := Mouse{Type: MousePress, Button: MouseLeft, Down: true}
	drag := Mouse{Type: MouseMotion, Button: MouseLeft, Down: true}
	motion := Mouse{Type: MouseMotion, Down: false}

	// No mouse mode → not reported.
	_, ok := press.Bytes(0)
	assert.False(t, ok)

	// X10 only reports presses.
	_, ok = press.Bytes(emu.ModeMouseX10)
	assert.True(t, ok)
	_, ok = drag.Bytes(emu.ModeMouseX10)
	assert.False(t, ok)

	// Button mode drops all motion.
	_, ok = press.Bytes(emu.ModeMouseButton)
	assert.True(t, ok)
	_, ok = drag.Bytes(emu.ModeMouseButton)
	assert.False(t, ok)
	_, ok = motion.Bytes(emu.ModeMouseButton)
	assert.False(t, ok)

	// Motion mode (1002) reports drag but not no-button motion.
	_, ok = press.Bytes(emu.ModeMouseMotion)
	assert.True(t, ok)
	_, ok = drag.Bytes(emu.ModeMouseMotion)
	assert.True(t, ok)
	_, ok = motion.Bytes(emu.ModeMouseMotion)
	assert.False(t, ok)

	// Many mode reports everything.
	_, ok = drag.Bytes(emu.ModeMouseMany)
	assert.True(t, ok)
	_, ok = motion.Bytes(emu.ModeMouseMany)
	assert.True(t, ok)
}

// TestSGRCrossFormat verifies that an SGR-parsed event can be
// serialized to X10 (within coordinate limits) and vice versa, and
// that the semantic Mouse value is consistent.
func TestSGRCrossFormat(t *testing.T) {
	cases := []struct {
		sgr   string
		mouse Mouse
	}{
		{
			sgr: "\x1b[<0;1;1M",
			mouse: Mouse{
				Type:   MousePress,
				Button: MouseLeft,
				Down:   true,
			},
		},
		{
			sgr: "\x1b[<2;11;21M",
			mouse: Mouse{
				Type:   MousePress,
				Button: MouseRight,
				Down:   true,
			},
		},
	}

	for _, tc := range cases {
		t.Run(tc.sgr, func(t *testing.T) {
			msg, _ := Read([]byte(tc.sgr))
			mouse, ok := msg.(Mouse)
			require.True(t, ok)
			assert.Equal(t, tc.mouse.Type, mouse.Type)
			assert.Equal(t, tc.mouse.Button, mouse.Button)
			assert.Equal(t, tc.mouse.Down, mouse.Down)

			// X10 round-trip: parse the X10 output and compare.
			x10, ok := mouse.Bytes(emu.ModeMouseMany)
			require.True(t, ok)
			msg2, _ := Read(x10)
			mouse2, ok := msg2.(Mouse)
			require.True(t, ok)
			assert.Equal(
				t, mouse.Button, mouse2.Button,
				fmt.Sprintf(
					"button mismatch for %q",
					tc.sgr,
				),
			)
			assert.Equal(t, mouse.Type, mouse2.Type)
			assert.Equal(t, mouse.C, mouse2.C)
			assert.Equal(t, mouse.R, mouse2.R)
		})
	}
}
