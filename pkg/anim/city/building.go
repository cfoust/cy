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

var buildingUvs = []gl.Vec2{
	{0, 0},
	{1.0, 0},
	{0.0, 1.0},
	{1.0, 1.0},
}

var buildingFaces = [][3]int{
	{2, 1, 0},
	{1, 2, 3},
}

type buildingShader struct {
	face     int
	Position gl.Vec3
	Size     gl.Vec2
}

var _ R.Shader = (*buildingShader)(nil)
var _ R.Drawable = (*buildingShader)(nil)

func (b *buildingShader) Vertex(
	camera *R.Camera, face, index int, vertex gl.Vec3,
) (out gl.Vec4) {
	out = vertex.Vec4(1)
	// This could be cleaned up but it's easier to understand this way
	// > Scale the flat (ie on xz plane) mesh to correct size
	out = gl.Scale3D(b.Size[0]/2, 1, b.Size[1]).Mul4x1(out)
	// > Orient it along the xy plane instead
	out = gl.HomogRotate3DX(math.Pi / 2).Mul4x1(out)
	// > Move the face outwards
	out = gl.Translate3D(0, 0, b.Size[0]/2).Mul4x1(out)
	// > Rotate it to its destination around y axis
	out = gl.HomogRotate3DY(float32(b.face) * math.Pi / 2).Mul4x1(out)
	// > Move the whole object to destination in world space
	out = gl.
		Translate3D(b.Position[0], b.Size[1], b.Position[2]).
		Mul4x1(out)
	return camera.Transform(out.Vec3())
}

func (b *buildingShader) Fragment(
	i0, i1, i2 int,
	bary gl.Vec3,
) (glyph emu.Glyph, discard bool) {
	glyph = emu.EmptyGlyph()
	glyph.Char = '⡀'
	glyph.FG = emu.LightYellow
	return
}

func (b *buildingShader) Draw(c *R.Context) {
	for i := 0; i < 4; i++ {
		b.face = i
		c.Triangles(b, buildingVerts, buildingFaces)
	}
}
