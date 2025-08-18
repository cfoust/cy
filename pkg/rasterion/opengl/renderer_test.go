package opengl

import (
	"context"
	"os"
	"runtime"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/require"
)

var globalRenderer *Renderer

// We can only run OpenGL in the main thread
func TestMain(m *testing.M) {
	// TODO(cfoust): 08/15/25 is this necessary?
	runtime.LockOSThread()
	defer runtime.UnlockOSThread()

	globalRenderer = NewRenderer()

	ctx, cancel := context.WithCancel(context.Background())

	testDone := make(chan int, 1)
	go func() {
		exitCode := m.Run()
		cancel()
		testDone <- exitCode
	}()

	globalRenderer.Poll(ctx)
	globalRenderer.Shutdown()
	os.Exit(<-testDone)
}

// TestRenderer tests the complete renderer functionality using the public ContextHandle API
func TestRenderer(t *testing.T) {
	ctx := context.Background()

	// Test 1: Create single context and basic operations
	handle, err := globalRenderer.NewContext(ctx, geom.Size{R: 256, C: 512})
	require.NoError(t, err, "Failed to create context")

	t.Logf("Created context successfully")

	// Test 2: Compile shader
	fragmentSource := `#version 330 core
out vec4 frag_color;

uniform float t;
uniform vec4 viewport;
uniform vec3 free_camera_target;
uniform vec2 free_camera_orbit;
uniform float free_camera_zoom;

void main() {
    vec2 uv = gl_FragCoord.xy / viewport.zw;
    vec3 color = vec3(uv.x, uv.y, sin(t));
    frag_color = vec4(color, 1.0);
}`

	err = handle.CompileShader(ctx, fragmentSource)
	require.NoError(t, err, "Failed to compile shader")

	t.Logf("Successfully compiled fragment shader")

	// Test 3: Render using ContextHandle (now returns both color and glyph data)
	colorPixels, glyphPixels, err := handle.Render(ctx, RenderParams{
		ViewportSize:     geom.Size{R: 256, C: 512}, // height=256, width=512
		Time:             1.5,
		FreeCameraTarget: [3]float32{0, 0, 0},
		FreeCameraOrbit:  [2]float32{0.25, -0.125},
		FreeCameraZoom:   1.0,
	})
	require.NoError(t, err, "Failed to render")

	expectedPixelCount := 256 * 512 * 4 // width * height * RGBA (aspect ratio corrected)
	require.Len(t, colorPixels, expectedPixelCount, "Unexpected color pixel count")
	require.Len(t, glyphPixels, expectedPixelCount, "Unexpected glyph pixel count")

	// Check that we have non-zero color pixels (indicating rendering worked)
	hasNonZeroColor := false
	for i := 0; i < len(colorPixels); i += 4 {
		if colorPixels[i] != 0 || colorPixels[i+1] != 0 || colorPixels[i+2] != 0 {
			hasNonZeroColor = true
			break
		}
	}
	require.True(t, hasNonZeroColor, "All color pixels are zero - rendering may have failed")

	// Verify glyph data is present (could be zero values, which is valid)
	require.NotNil(t, glyphPixels, "Glyph pixels should not be nil")

	// Test 4: Render with different viewport size (test resizing)
	colorPixels2, glyphPixels2, err := handle.Render(ctx, RenderParams{
		ViewportSize: geom.Size{
			R: 128,
			C: 256,
		}, // height=128, width=256 (smaller)
		Time:             2.0,
		FreeCameraTarget: [3]float32{1, 1, 1},
		FreeCameraOrbit:  [2]float32{0.5, 0.25},
		FreeCameraZoom:   2.0,
	})
	require.NoError(t, err, "Failed to render with new size")

	expectedPixelCount2 := 128 * 256 * 4 // width * height * RGBA (aspect ratio corrected)
	require.Len(t, colorPixels2, expectedPixelCount2, "Unexpected color pixel count for resized render")
	require.Len(t, glyphPixels2, expectedPixelCount2, "Unexpected glyph pixel count for resized render")

	// Test 5: Test shader compilation error
	err = handle.CompileShader(ctx, "invalid shader code")
	require.Error(t, err, "Expected shader compilation error, but got none")

	t.Logf("Correctly caught shader compilation error: %v", err)

	// Test 6: Test context cancellation
	cancelCtx, cancel := context.WithCancel(ctx)
	cancel() // Cancel immediately

	_, _, err = handle.Render(cancelCtx, RenderParams{
		ViewportSize:     geom.Size{R: 100, C: 100},
		Time:             0.0,
		FreeCameraTarget: [3]float32{0, 0, 0},
		FreeCameraOrbit:  [2]float32{0, 0},
		FreeCameraZoom:   1.0,
	})
	require.Error(t, err, "Expected context cancellation error")
	require.Equal(t, context.Canceled, err, "Expected context.Canceled error")

	t.Logf("Correctly handled context cancellation: %v", err)

	// Test 7: Test multiple contexts (create additional contexts)
	handle2, err := globalRenderer.NewContext(ctx, geom.Size{R: 128, C: 128})
	require.NoError(t, err, "Failed to create second context")

	handle3, err := globalRenderer.NewContext(ctx, geom.Size{R: 64, C: 64})
	require.NoError(t, err, "Failed to create third context")

	t.Logf("Created multiple contexts successfully")

	// Compile shader in additional contexts
	err = handle2.CompileShader(ctx, fragmentSource)
	require.NoError(t, err, "Failed to compile shader in context 2")

	err = handle3.CompileShader(ctx, fragmentSource)
	require.NoError(t, err, "Failed to compile shader in context 3")

	// Render in additional contexts
	_, _, err = handle2.Render(ctx, RenderParams{
		ViewportSize:     geom.Size{R: 128, C: 128},
		Time:             1.0,
		FreeCameraTarget: [3]float32{0, 0, 0},
		FreeCameraOrbit:  [2]float32{0, 0},
		FreeCameraZoom:   1.0,
	})
	require.NoError(t, err, "Failed to render in context 2")

	_, _, err = handle3.Render(ctx, RenderParams{
		ViewportSize:     geom.Size{R: 64, C: 64},
		Time:             2.0,
		FreeCameraTarget: [3]float32{1, 1, 1},
		FreeCameraOrbit:  [2]float32{0.5, 0.5},
		FreeCameraZoom:   2.0,
	})
	require.NoError(t, err, "Failed to render in context 3")

	t.Logf("Successfully rendered in multiple contexts")

	// Test 8: Clean up all contexts
	err = handle.Destroy(ctx)
	require.NoError(t, err, "Failed to destroy first context")

	err = handle2.Destroy(ctx)
	require.NoError(t, err, "Failed to destroy second context")

	err = handle3.Destroy(ctx)
	require.NoError(t, err, "Failed to destroy third context")

	t.Logf("Successfully destroyed all contexts")

	// Test 9: Verify contexts are destroyed (should fail)
	err = handle.CompileShader(ctx, fragmentSource)
	require.Error(
		t,
		err,
		"Expected error when using destroyed context, but got none",
	)

	t.Logf("Correctly verified context destruction: %v", err)

	// Test 10: Test Screen implementation
	t.Logf("Testing Screen implementation...")

	// Create a simple test shader
	screenFragmentSource := `#version 330 core
out vec4 frag_color;

uniform float t;
uniform vec4 viewport;

void main() {
    vec2 uv = gl_FragCoord.xy / viewport.zw;
    vec3 color = vec3(uv.x, uv.y, sin(t * 3.14159));
    frag_color = vec4(color, 1.0);
}`

	// Create a screen
	screen := NewScreen(ctx, globalRenderer, screenFragmentSource)
	require.NotNil(t, screen)

	// Test initial state
	state := screen.State()
	require.NotNil(t, state)
	require.Equal(t, geom.Size{R: 24, C: 80}, state.Image.Size())

	// Test resize
	err = screen.Resize(geom.Size{R: 10, C: 20})
	require.NoError(t, err)

	state = screen.State()
	require.Equal(t, geom.Size{R: 10, C: 20}, state.Image.Size())

	// Give it a moment to render
	time.Sleep(100 * time.Millisecond)

	// Test with invalid shader
	invalidScreen := NewScreen(ctx, globalRenderer, "invalid shader")
	require.NotNil(t, invalidScreen)

	// Give it a moment to try to compile
	time.Sleep(100 * time.Millisecond)

	state = invalidScreen.State()
	require.NotNil(t, state)

	// Cleanup
	screen.Kill()
	invalidScreen.Kill()

	t.Logf("Screen test completed successfully")
}
