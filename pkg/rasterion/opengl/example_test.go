package opengl_test

import (
	"testing"

	"github.com/cfoust/cy/pkg/rasterion/opengl"

	"github.com/go-gl/gl/v3.3-core/gl"
)

const (
	vertexShaderSource = `#version 330 core
layout (location = 0) in vec2 aPos;
out vec2 fragCoord;

void main()
{
    fragCoord = aPos * 0.5 + 0.5;
    gl_Position = vec4(aPos.x, aPos.y, 0.0, 1.0);
}` + "\x00"

	fragmentShaderSource = `#version 330 core
in vec2 fragCoord;
out vec4 FragColor;

uniform float time;
uniform vec2 resolution;

void main()
{
    vec2 uv = fragCoord;
    vec3 color = 0.5 + 0.5 * cos(time + uv.xyx + vec3(0, 2, 4));
    FragColor = vec4(color, 1.0);
}` + "\x00"
)

func TestOpenGLRenderer(t *testing.T) {
	renderer, err := opengl.NewRenderer(512, 512)
	if err != nil {
		t.Fatalf("Failed to create renderer: %v", err)
	}
	defer renderer.Destroy()

	// Compile shaders
	vertexShader, err := opengl.CompileShader(
		vertexShaderSource,
		gl.VERTEX_SHADER,
	)
	if err != nil {
		t.Fatalf("Failed to compile vertex shader: %v", err)
	}
	defer vertexShader.Delete()

	fragmentShader, err := opengl.CompileShader(
		fragmentShaderSource,
		gl.FRAGMENT_SHADER,
	)
	if err != nil {
		t.Fatalf("Failed to compile fragment shader: %v", err)
	}
	defer fragmentShader.Delete()

	// Create program
	program, err := opengl.CreateProgram(vertexShader, fragmentShader)
	if err != nil {
		t.Fatalf("Failed to create program: %v", err)
	}
	defer program.Delete()

	renderer.SetShaderProgram(program)

	// Set uniforms
	program.Use()
	timeLocation := program.GetUniformLocation("time")
	resolutionLocation := program.GetUniformLocation("resolution")

	program.SetUniform1f(timeLocation, 1.0)
	program.SetUniform2f(
		resolutionLocation,
		float32(renderer.Width()),
		float32(renderer.Height()),
	)

	// Clear and render
	renderer.Clear(0.0, 0.0, 0.0, 1.0)

	err = renderer.Render()
	if err != nil {
		t.Fatalf("Failed to render: %v", err)
	}

	// Read back pixels
	pixels, err := renderer.ReadPixels()
	if err != nil {
		t.Fatalf("Failed to read pixels: %v", err)
	}

	expectedSize := renderer.Width() * renderer.Height() * 4 // RGBA
	if len(pixels) != expectedSize {
		t.Fatalf("Expected %d pixels, got %d", expectedSize, len(pixels))
	}

	// Check that we have non-zero pixels (indicating rendering worked)
	hasNonZero := false
	for i := 0; i < len(pixels); i += 4 {
		if pixels[i] != 0 || pixels[i+1] != 0 || pixels[i+2] != 0 {
			hasNonZero = true
			break
		}
	}

	if !hasNonZero {
		t.Fatal("All pixels are black, rendering may have failed")
	}

	t.Logf(
		"Successfully rendered %dx%d texture with %d bytes",
		renderer.Width(),
		renderer.Height(),
		len(pixels),
	)
}
