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

type Framebuffer struct {
	id      uint32
	texture *Texture
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
	texture := NewTexture(width, height)

	var fbo uint32
	gl.GenFramebuffers(1, &fbo)
	gl.BindFramebuffer(gl.FRAMEBUFFER, fbo)

	gl.FramebufferTexture2D(
		gl.FRAMEBUFFER,
		gl.COLOR_ATTACHMENT0,
		gl.TEXTURE_2D,
		texture.id,
		0,
	)

	status := gl.CheckFramebufferStatus(gl.FRAMEBUFFER)
	if status != gl.FRAMEBUFFER_COMPLETE {
		gl.DeleteFramebuffers(1, &fbo)
		texture.Delete()
		return nil, fmt.Errorf("framebuffer is not complete: %d", status)
	}

	gl.BindFramebuffer(gl.FRAMEBUFFER, 0)

	return &Framebuffer{
		id:      fbo,
		texture: texture,
	}, nil
}

func (f *Framebuffer) Bind() {
	gl.BindFramebuffer(gl.FRAMEBUFFER, f.id)
	gl.Viewport(0, 0, f.texture.width, f.texture.height)
}

func (f *Framebuffer) Unbind() {
	gl.BindFramebuffer(gl.FRAMEBUFFER, 0)
}

func (f *Framebuffer) Delete() {
	if f.id != 0 {
		gl.DeleteFramebuffers(1, &f.id)
		f.id = 0
	}
	if f.texture != nil {
		f.texture.Delete()
		f.texture = nil
	}
}

func (f *Framebuffer) Texture() *Texture {
	return f.texture
}

func (f *Framebuffer) Clear(r, g, b, a float32) {
	f.Bind()
	gl.ClearColor(r, g, b, a)
	gl.Clear(gl.COLOR_BUFFER_BIT)
	f.Unbind()
}

func (f *Framebuffer) ReadPixels() ([]byte, error) {
	if f.id == 0 {
		return nil, fmt.Errorf("framebuffer has been deleted")
	}

	f.Bind()
	defer f.Unbind()

	data := make([]byte, f.texture.width*f.texture.height*4) // RGBA
	gl.ReadPixels(
		0,
		0,
		f.texture.width,
		f.texture.height,
		gl.RGBA,
		gl.UNSIGNED_BYTE,
		gl.Ptr(data),
	)

	return data, nil
}
