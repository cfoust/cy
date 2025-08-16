package opengl

import (
	"context"
	"os"
	"runtime"
	"testing"

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
	handle := globalRenderer.NewContext(ctx, geom.Size{R: 256, C: 512})
	require.NoError(t, handle.Error(), "Failed to create context")

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

	err := handle.CompileShader(ctx, fragmentSource)
	require.NoError(t, err, "Failed to compile shader")

	t.Logf("Successfully compiled fragment shader")

	// Test 3: Render using ContextHandle
	pixels, err := handle.Render(ctx,
		geom.Size{R: 256, C: 512}, // height=256, width=512
		1.5,                       // time
		[3]float32{0, 0, 0},       // freeCameraTarget
		[2]float32{0.25, -0.125},  // freeCameraOrbit
		1.0,                       // freeCameraZoom
	)
	require.NoError(t, err, "Failed to render")

	expectedPixelCount := 512 * 256 * 4 // width * height * RGBA
	require.Len(t, pixels, expectedPixelCount, "Unexpected pixel count")

	// Check that we have non-zero pixels (indicating rendering worked)
	hasNonZero := false
	for i := 0; i < len(pixels); i += 4 {
		if pixels[i] != 0 || pixels[i+1] != 0 || pixels[i+2] != 0 {
			hasNonZero = true
			break
		}
	}
	require.True(t, hasNonZero, "All pixels are black, rendering may have failed")

	// Test 4: Render with different viewport size (test resizing)
	pixels2, err := handle.Render(ctx,
		geom.Size{R: 128, C: 256}, // height=128, width=256 (smaller)
		2.0,                       // time
		[3]float32{1, 1, 1},       // freeCameraTarget
		[2]float32{0.5, 0.25},     // freeCameraOrbit
		2.0,                       // freeCameraZoom
	)
	require.NoError(t, err, "Failed to render with new size")

	expectedPixelCount2 := 256 * 128 * 4 // width * height * RGBA
	require.Len(t, pixels2, expectedPixelCount2, "Unexpected pixel count for resized render")

	// Test 5: Test shader compilation error
	err = handle.CompileShader(ctx, "invalid shader code")
	require.Error(t, err, "Expected shader compilation error, but got none")

	t.Logf("Correctly caught shader compilation error: %v", err)

	// Test 6: Test context cancellation
	cancelCtx, cancel := context.WithCancel(ctx)
	cancel() // Cancel immediately

	_, err = handle.Render(cancelCtx,
		geom.Size{R: 100, C: 100},
		0.0,
		[3]float32{0, 0, 0},
		[2]float32{0, 0},
		1.0,
	)
	require.Error(t, err, "Expected context cancellation error")
	require.Equal(t, context.Canceled, err, "Expected context.Canceled error")

	t.Logf("Correctly handled context cancellation: %v", err)

	// Test 7: Test multiple contexts (create additional contexts)
	handle2 := globalRenderer.NewContext(ctx, geom.Size{R: 128, C: 128})
	require.NoError(t, handle2.Error(), "Failed to create second context")

	handle3 := globalRenderer.NewContext(ctx, geom.Size{R: 64, C: 64})
	require.NoError(t, handle3.Error(), "Failed to create third context")

	t.Logf("Created multiple contexts successfully")

	// Compile shader in additional contexts
	err = handle2.CompileShader(ctx, fragmentSource)
	require.NoError(t, err, "Failed to compile shader in context 2")

	err = handle3.CompileShader(ctx, fragmentSource)
	require.NoError(t, err, "Failed to compile shader in context 3")

	// Render in additional contexts
	pixels3, err := handle2.Render(ctx,
		geom.Size{R: 128, C: 128},
		1.0,
		[3]float32{0, 0, 0},
		[2]float32{0, 0},
		1.0,
	)
	require.NoError(t, err, "Failed to render in context 2")
	require.Len(t, pixels3, 128*128*4, "Unexpected pixel count for context 2")

	pixels4, err := handle3.Render(ctx,
		geom.Size{R: 64, C: 64},
		2.0,
		[3]float32{1, 1, 1},
		[2]float32{0.5, 0.5},
		2.0,
	)
	require.NoError(t, err, "Failed to render in context 3")
	require.Len(t, pixels4, 64*64*4, "Unexpected pixel count for context 3")

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
	require.Error(t, err, "Expected error when using destroyed context, but got none")

	t.Logf("Correctly verified context destruction: %v", err)
}
