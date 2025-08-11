package opengl

import (
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/geom"
)

func TestNewScreen(t *testing.T) {
	// Simple fragment shader for testing
	fragmentSource := `#version 330 core
out vec4 FragColor;

uniform float iTime;
uniform vec2 iResolution;

void main() {
    vec2 uv = gl_FragCoord.xy / iResolution.xy;
    vec3 color = vec3(uv.x, uv.y, sin(iTime));
    FragColor = vec4(color, 1.0);
}`

	screen, err := NewScreen(fragmentSource, 80, 24)
	if err != nil {
		t.Fatalf("Failed to create OpenGL screen: %v", err)
	}
	defer screen.Kill()

	// Give the rendering goroutine a moment to initialize
	time.Sleep(100 * time.Millisecond)

	// Test basic functionality
	state := screen.State()
	if state == nil {
		t.Fatal("State() returned nil")
	}

	if state.Image.Size() != (geom.Vec2{R: 24, C: 80}) {
		t.Errorf("Expected size {24 80}, got %v", state.Image.Size())
	}

	// Test resize
	err = screen.Resize(geom.Vec2{R: 30, C: 30})
	if err != nil {
		t.Errorf("Resize failed: %v", err)
	}

	// Give resize time to complete
	time.Sleep(50 * time.Millisecond)

	state = screen.State()
	if state.Image.Size() != (geom.Vec2{R: 30, C: 100}) {
		t.Errorf("After resize, expected size {30 100}, got %v", state.Image.Size())
	}

	t.Logf("%+v", state.Image)

	//// Test subscription
	//ctx, cancel := context.WithCancel(context.Background())
	//defer cancel()

	//updater := screen.Subscribe(ctx)
	//if updater == nil {
	//t.Error("Subscribe returned nil updater")
	//}
}

func TestScreenKill(t *testing.T) {
	fragmentSource := `#version 330 core
out vec4 FragColor;
void main() { FragColor = vec4(1.0, 0.0, 0.0, 1.0); }`

	screen, err := NewScreen(fragmentSource, 80, 24)
	if err != nil {
		t.Fatalf("Failed to create OpenGL screen: %v", err)
	}

	screen.Kill()

	// After kill, resize should fail
	err = screen.Resize(geom.Vec2{R: 30, C: 100})
	if err == nil {
		t.Error("Expected resize to fail after Kill(), but it succeeded")
	}
}
