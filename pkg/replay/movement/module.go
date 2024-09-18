package movement

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/replay/motion"
)

type Highlight struct {
	// Whether this Highlight is in screen space or in the reference frame
	// of the Movement.
	Screen   bool
	From, To geom.Vec2
	FG, BG   emu.Color
}

type Movement interface {
	motion.Movable

	// Snap repositions the cursor to be in a valid cell in the coordinate
	// system of the Movement. This is only useful for flowMovement and
	// does nothing in image mode.
	Snap()

	// Cursor returns the position of the cursor in the coordinate system
	// of the Movement.
	Cursor() geom.Vec2

	// MoveCursorX moves the cursor horizontally by `delta` (positive
	// numbers to the right).
	MoveCursorX(delta int)

	// MoveCursorY moves the cursor vertically by `delta` (positive numbers
	// go down).
	MoveCursorY(delta int)

	// ReadString reads string data between two coordinates `start` and
	// `end` in the reference frame of the Movement.
	ReadString(start, end geom.Vec2) string

	// Resize resizes the Movement and makes any necessary viewport
	// adjustments.
	Resize(geom.Size)

	// ScrollBottom moves the cursor to the last possible line.
	ScrollBottom()

	// ScrollTop moves the cursor to the first line.
	ScrollTop()

	// ScrollXDelta scrolls the viewport horizontally (positive numbers go
	// to the right.)
	ScrollXDelta(delta int)

	// ScrollYDelta scrolls the viewport vertically (positive numbers go
	// down.)
	ScrollYDelta(delta int)

	// Goto moves the cursor to `location`, which is a coordinate in the
	// reference frame of the movement.
	Goto(location geom.Vec2)

	// View renders the Movement to a tty.State with the provided
	// `highlights` given in the reference frame of the movement.
	View(
		params *params.Parameters,
		state *tty.State,
		highlights []Highlight,
	)
}
