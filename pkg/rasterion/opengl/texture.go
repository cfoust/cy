package opengl

import (
	"fmt"

	"github.com/go-gl/gl/v3.3-core/gl"
)

type Texture struct {
	id     uint32
	width  int32
	height int32
}

// Framebuffer supports Multiple Render Targets (MRT) for both color and glyph output
type Framebuffer struct {
	id           uint32
	colorTexture *Texture // COLOR_ATTACHMENT0 - vec4 frag_color
	glyphTexture *Texture // COLOR_ATTACHMENT1 - vec3 glyph (stored as RGB)
}

func NewTexture(width, height int32) *Texture {
	var id uint32
	gl.GenTextures(1, &id)
	gl.BindTexture(gl.TEXTURE_2D, id)

	gl.TexImage2D(
		gl.TEXTURE_2D,
		0,
		gl.RGBA,
		width,
		height,
		0,
		gl.RGBA,
		gl.UNSIGNED_BYTE,
		nil,
	)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)

	gl.BindTexture(gl.TEXTURE_2D, 0)

	return &Texture{
		id:     id,
		width:  width,
		height: height,
	}
}

func (t *Texture) Bind() {
	gl.BindTexture(gl.TEXTURE_2D, t.id)
}

func (t *Texture) Unbind() {
	gl.BindTexture(gl.TEXTURE_2D, 0)
}

func (t *Texture) Delete() {
	if t.id != 0 {
		gl.DeleteTextures(1, &t.id)
		t.id = 0
	}
}

func (t *Texture) Width() int32 {
	return t.width
}

func (t *Texture) Height() int32 {
	return t.height
}

func (t *Texture) ReadPixels() ([]byte, error) {
	if t.id == 0 {
		return nil, fmt.Errorf("texture has been deleted")
	}

	t.Bind()
	defer t.Unbind()

	data := make([]byte, t.width*t.height*4) // RGBA
	gl.GetTexImage(gl.TEXTURE_2D, 0, gl.RGBA, gl.UNSIGNED_BYTE, gl.Ptr(data))

	return data, nil
}

func NewFramebuffer(width, height int32) (*Framebuffer, error) {
	colorTexture := NewTexture(width, height)
	glyphTexture := NewTexture(width, height)

	var fbo uint32
	gl.GenFramebuffers(1, &fbo)
	gl.BindFramebuffer(gl.FRAMEBUFFER, fbo)

	// Attach color texture to COLOR_ATTACHMENT0
	gl.FramebufferTexture2D(
		gl.FRAMEBUFFER,
		gl.COLOR_ATTACHMENT0,
		gl.TEXTURE_2D,
		colorTexture.id,
		0,
	)

	// Attach glyph texture to COLOR_ATTACHMENT1
	gl.FramebufferTexture2D(
		gl.FRAMEBUFFER,
		gl.COLOR_ATTACHMENT1,
		gl.TEXTURE_2D,
		glyphTexture.id,
		0,
	)

	// Set the draw buffers to write to both attachments
	drawBuffers := []uint32{gl.COLOR_ATTACHMENT0, gl.COLOR_ATTACHMENT1}
	gl.DrawBuffers(int32(len(drawBuffers)), &drawBuffers[0])

	status := gl.CheckFramebufferStatus(gl.FRAMEBUFFER)
	if status != gl.FRAMEBUFFER_COMPLETE {
		gl.DeleteFramebuffers(1, &fbo)
		colorTexture.Delete()
		glyphTexture.Delete()
		return nil, fmt.Errorf("framebuffer is not complete: %d", status)
	}

	gl.BindFramebuffer(gl.FRAMEBUFFER, 0)

	return &Framebuffer{
		id:           fbo,
		colorTexture: colorTexture,
		glyphTexture: glyphTexture,
	}, nil
}

func (f *Framebuffer) Bind() {
	gl.BindFramebuffer(gl.FRAMEBUFFER, f.id)
	gl.Viewport(0, 0, f.colorTexture.width, f.colorTexture.height)
}

func (f *Framebuffer) Unbind() {
	gl.BindFramebuffer(gl.FRAMEBUFFER, 0)
}

func (f *Framebuffer) Delete() {
	if f.id != 0 {
		gl.DeleteFramebuffers(1, &f.id)
		f.id = 0
	}
	if f.colorTexture != nil {
		f.colorTexture.Delete()
		f.colorTexture = nil
	}
	if f.glyphTexture != nil {
		f.glyphTexture.Delete()
		f.glyphTexture = nil
	}
}

func (f *Framebuffer) ColorTexture() *Texture {
	return f.colorTexture
}

func (f *Framebuffer) GlyphTexture() *Texture {
	return f.glyphTexture
}

// Legacy method for backward compatibility
func (f *Framebuffer) Texture() *Texture {
	return f.colorTexture
}

func (f *Framebuffer) Clear(r, g, b, a float32) {
	f.Bind()
	gl.ClearColor(r, g, b, a)
	gl.Clear(gl.COLOR_BUFFER_BIT)
	f.Unbind()
}

// ReadPixels returns both color and glyph data as float arrays
func (f *Framebuffer) ReadPixels() (colorData []float32, glyphData []float32, err error) {
	if f.id == 0 {
		return nil, nil, fmt.Errorf("framebuffer has been deleted")
	}

	f.Bind()
	defer f.Unbind()

	width := f.colorTexture.width
	height := f.colorTexture.height

	// Read color data from COLOR_ATTACHMENT0 as floats
	colorData = make([]float32, width*height*4) // RGBA
	gl.ReadBuffer(gl.COLOR_ATTACHMENT0)
	gl.ReadPixels(
		0, 0, width, height,
		gl.RGBA, gl.FLOAT,
		gl.Ptr(colorData),
	)

	// Read glyph data from COLOR_ATTACHMENT1 as floats (unclamped values)
	glyphData = make([]float32, width*height*3)
	gl.ReadBuffer(gl.COLOR_ATTACHMENT1)
	gl.ReadPixels(
		0, 0, width, height,
		gl.RGB, gl.FLOAT,
		gl.Ptr(glyphData),
	)

	return colorData, glyphData, nil
}
