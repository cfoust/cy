package opengl

import (
	"fmt"

	"github.com/go-gl/gl/v3.3-core/gl"
)

type Renderer struct {
	context     *Context
	framebuffer *Framebuffer
	program     *Program
	vao         uint32
	vbo         uint32
}

type QuadVertex struct {
	X, Y float32
}

func NewRenderer(width, height int) (*Renderer, error) {
	ctx, err := NewContext(width, height)
	if err != nil {
		return nil, fmt.Errorf("failed to create context: %w", err)
	}

	ctx.MakeCurrent()

	fbo, err := NewFramebuffer(int32(width), int32(height))
	if err != nil {
		ctx.Destroy()
		return nil, fmt.Errorf("failed to create framebuffer: %w", err)
	}

	// Create a fullscreen quad
	vertices := []QuadVertex{
		{-1.0, -1.0},
		{1.0, -1.0},
		{1.0, 1.0},
		{-1.0, 1.0},
	}

	var vao, vbo uint32
	gl.GenVertexArrays(1, &vao)
	gl.GenBuffers(1, &vbo)

	gl.BindVertexArray(vao)
	gl.BindBuffer(gl.ARRAY_BUFFER, vbo)
	gl.BufferData(
		gl.ARRAY_BUFFER,
		len(vertices)*8,
		gl.Ptr(vertices),
		gl.STATIC_DRAW,
	)

	gl.VertexAttribPointerWithOffset(0, 2, gl.FLOAT, false, 8, 0)
	gl.EnableVertexAttribArray(0)

	gl.BindVertexArray(0)
	gl.BindBuffer(gl.ARRAY_BUFFER, 0)

	return &Renderer{
		context:     ctx,
		framebuffer: fbo,
		vao:         vao,
		vbo:         vbo,
	}, nil
}

func (r *Renderer) SetShaderProgram(program *Program) {
	r.program = program
}

func (r *Renderer) Render() error {
	if r.program == nil {
		return fmt.Errorf("no shader program set")
	}

	r.context.MakeCurrent()
	r.framebuffer.Bind()

	r.program.Use()
	gl.BindVertexArray(r.vao)
	gl.DrawArrays(gl.TRIANGLE_FAN, 0, 4)
	gl.BindVertexArray(0)

	r.framebuffer.Unbind()

	return nil
}

func (r *Renderer) ReadPixels() ([]byte, error) {
	return r.framebuffer.ReadPixels()
}

func (r *Renderer) GetTexture() *Texture {
	return r.framebuffer.Texture()
}

func (r *Renderer) Clear(r_, g, b, a float32) {
	r.framebuffer.Clear(r_, g, b, a)
}

func (r *Renderer) Width() int {
	return r.context.Width()
}

func (r *Renderer) Height() int {
	return r.context.Height()
}

func (r *Renderer) Destroy() {
	if r.vao != 0 {
		gl.DeleteVertexArrays(1, &r.vao)
		r.vao = 0
	}
	if r.vbo != 0 {
		gl.DeleteBuffers(1, &r.vbo)
		r.vbo = 0
	}
	if r.framebuffer != nil {
		r.framebuffer.Delete()
		r.framebuffer = nil
	}
	if r.program != nil {
		r.program.Delete()
		r.program = nil
	}
	if r.context != nil {
		r.context.Destroy()
		r.context = nil
	}
}
