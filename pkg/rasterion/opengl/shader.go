package opengl

import (
	"fmt"
	"strings"

	"github.com/go-gl/gl/v3.3-core/gl"
)

type Shader struct {
	id uint32
}

type Program struct {
	id uint32
}

func CompileShader(source string, shaderType uint32) (*Shader, error) {
	shader := gl.CreateShader(shaderType)

	csources, free := gl.Strs(source)
	gl.ShaderSource(shader, 1, csources, nil)
	free()
	gl.CompileShader(shader)

	var status int32
	gl.GetShaderiv(shader, gl.COMPILE_STATUS, &status)
	if status == gl.FALSE {
		var logLength int32
		gl.GetShaderiv(shader, gl.INFO_LOG_LENGTH, &logLength)

		log := strings.Repeat("\x00", int(logLength+1))
		gl.GetShaderInfoLog(shader, logLength, nil, gl.Str(log))

		return nil, fmt.Errorf("failed to compile shader: %v", log)
	}

	return &Shader{id: shader}, nil
}

func (s *Shader) Delete() {
	if s.id != 0 {
		gl.DeleteShader(s.id)
		s.id = 0
	}
}

func CreateProgram(vertexShader, fragmentShader *Shader) (*Program, error) {
	program := gl.CreateProgram()

	gl.AttachShader(program, vertexShader.id)
	gl.AttachShader(program, fragmentShader.id)
	gl.LinkProgram(program)

	var status int32
	gl.GetProgramiv(program, gl.LINK_STATUS, &status)
	if status == gl.FALSE {
		var logLength int32
		gl.GetProgramiv(program, gl.INFO_LOG_LENGTH, &logLength)

		log := strings.Repeat("\x00", int(logLength+1))
		gl.GetProgramInfoLog(program, logLength, nil, gl.Str(log))

		return nil, fmt.Errorf("failed to link program: %v", log)
	}

	return &Program{id: program}, nil
}

func (p *Program) Use() {
	gl.UseProgram(p.id)
}

func (p *Program) Delete() {
	if p.id != 0 {
		gl.DeleteProgram(p.id)
		p.id = 0
	}
}

func (p *Program) GetUniformLocation(name string) int32 {
	return gl.GetUniformLocation(p.id, gl.Str(name+"\x00"))
}

func (p *Program) SetUniform1f(location int32, value float32) {
	gl.Uniform1f(location, value)
}

func (p *Program) SetUniform2f(location int32, x, y float32) {
	gl.Uniform2f(location, x, y)
}

func (p *Program) SetUniform3f(location int32, x, y, z float32) {
	gl.Uniform3f(location, x, y, z)
}

func (p *Program) SetUniform4f(location int32, x, y, z, w float32) {
	gl.Uniform4f(location, x, y, z, w)
}

func (p *Program) SetUniform1i(location int32, value int32) {
	gl.Uniform1i(location, value)
}
