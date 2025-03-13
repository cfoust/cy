package rasterion

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom/image"

	gl "github.com/go-gl/mathgl/mgl32"
)

type VertexShader interface {
	Vertex(
		camera *Camera,
		i0, i1, i2 int,
		v0, v1, v2 gl.Vec3,
	) (o0, o1, o2 gl.Vec4)
}

type DefaultVertexShader struct{}

var _ VertexShader = (*DefaultVertexShader)(nil)

func (v DefaultVertexShader) Vertex(
	camera *Camera,
	i0, i1, i2 int,
	v0, v1, v2 gl.Vec3,
) (o0, o1, o2 gl.Vec4) {
	o0 = camera.Transform(v0)
	o1 = camera.Transform(v1)
	o2 = camera.Transform(v2)
	return
}

type TransformShader struct {
	M gl.Mat4
}

var _ VertexShader = (*TransformShader)(nil)

func (t TransformShader) Transform(v gl.Vec3) gl.Vec4 {
	return t.M.Mul4x1(v.Vec4(1))
}

func (t TransformShader) Vertex(
	camera *Camera,
	i0, i1, i2 int,
	v0, v1, v2 gl.Vec3,
) (o0, o1, o2 gl.Vec4) {
	o0 = camera.Transform(t.Transform(v0).Vec3())
	o1 = camera.Transform(t.Transform(v1).Vec3())
	o2 = camera.Transform(t.Transform(v2).Vec3())
	return
}

type FragmentShader interface {
	Fragment(
		i0, i1, i2 int,
		bary gl.Vec3,
	) (glyph emu.Glyph, discard bool)
}

func Interpolate(bary gl.Vec3, v0, v1, v2 float32) float32 {
	return bary[0]*v0 + bary[1]*v1 + bary[2]*v2
}

func InterpolateVec2(bary gl.Vec3, v0, v1, v2 gl.Vec2) gl.Vec2 {
	v0 = v0.Mul(bary[0])
	v1 = v1.Mul(bary[1])
	v2 = v2.Mul(bary[2])
	return v0.Add(v1).Add(v2)
}

// Texture samples a texture at the given coordinate. The members of `uv`
// should be in the range [0..1].
func Texture(texture image.Image, uv gl.Vec2) emu.Glyph {
	size := texture.Size()

	if size.C == 0 || size.R == 0 {
		return emu.EmptyGlyph()
	}

	uv[0] = gl.Clamp(uv[0], 0, 1.0)
	uv[1] = gl.Clamp(uv[1], 0, 1.0)

	col := int(uv[0] * float32(size.C-1))
	row := int(uv[1] * float32(size.R-1))

	if col >= size.C || row >= size.R {
		return emu.EmptyGlyph()
	}

	return texture[row][col]
}

type Shader interface {
	VertexShader
	FragmentShader
}
