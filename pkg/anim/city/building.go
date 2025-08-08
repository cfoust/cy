package city

import (
	"math"

	"github.com/cfoust/cy/pkg/emu"
	R "github.com/cfoust/cy/pkg/rasterion"

	gl "github.com/go-gl/mathgl/mgl32"
)

var buildingVerts = []gl.Vec3{
	{-1, 0, -1}, // Back-left
	{1, 0, -1},  // Back-right
	{-1, 0, 1},  // Front-left
	{1, 0, 1},   // Front-right
}

var buildingFaces = [][3]int{
	{2, 1, 0},
	{1, 2, 3},
}

type buildingShader struct {
	face     int
	Type     int
	Position gl.Vec3
	Size     gl.Vec2
}

var _ R.Shader = (*buildingShader)(nil)
var _ R.Drawable = (*buildingShader)(nil)

func (b *buildingShader) Vertex(
	camera *R.Camera, face, index int, vertex gl.Vec3,
) (out gl.Vec4) {
	out = vertex.Vec4(1)
	width := b.Size[0] / 2
	height := b.Size[1] + float32(b.Type)*0.1

	// Draw the top
	if b.face == 4 {
		out = gl.Scale3D(width, 1, width).
			Mul4x1(out)
		out = gl.Translate3D(0, height, 0).Mul4x1(out)
	} else {
		// This could be cleaned up but it's easier to understand this way
		// > Scale the flat (ie on xz plane) mesh to correct size
		out = gl.Scale3D(
			b.Size[0]/2,
			1,
			height,
		).Mul4x1(out)
		// > Orient it along the xy plane instead
		out = gl.HomogRotate3DX(math.Pi / 2).Mul4x1(out)
		// > Move the face outwards
		out = gl.Translate3D(0, 0, width).Mul4x1(out)
		// > Rotate it to its destination around y axis
		out = gl.
			HomogRotate3DY(float32(b.face) * math.Pi / 2).
			Mul4x1(out)
	}

	// > Move the whole object to destination in world space
	out = gl.
		Translate3D(b.Position[0], height, b.Position[2]).
		Mul4x1(out)
	return camera.Transform(out.Vec3())
}

var (
	lights = []rune{
		'⡀',
		'⡁',
		'⡂',
		'⡃',
		'⡄',
		'⡅',
		'⡆',
		'⡇',
		'⡈',
		'⡉',
		'⡊',
		'⡋',
		'⡌',
	}
	numLights = len(lights)
)

func (b *buildingShader) Fragment(
	i0, i1, i2 int,
	bary gl.Vec3,
) (glyph emu.Glyph, discard bool) {
	glyph = emu.EmptyGlyph()

	if b.face == 4 {
		glyph.Char = '⡁'
		glyph.FG = emu.DarkGrey
		return
	}

	m := gl.Mat2x3FromCols(
		screenUvs[i0],
		screenUvs[i1],
		screenUvs[i2],
	)
	uv := m.Mul3x1(bary)

	// Apply light pattern to building sides
	index := (int(uv[0]*float32(numLights)) + b.Type) % numLights
	index = (index + int(uv[1]*5)) % numLights
	glyph.Char = lights[index]

	// Fade color
	glyph.FG = emu.ANSIColor(b.Type + 1)
	if uv[1] > 0.5 {
		glyph.FG = emu.ANSIColor(b.Type + 9)
	}

	return
}

func (b *buildingShader) Draw(c *R.Context) {
	for i := 0; i < 5; i++ {
		b.face = i
		c.Triangles(b, buildingVerts, buildingFaces)
	}
}
