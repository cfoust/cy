package opengl

import (
	"runtime"
	"testing"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/stretchr/testify/require"
)

// TestRendererHandleFunctions tests the core renderer functionality by calling handle* functions directly
// This must be run individually due to OpenGL main thread requirements
func TestRendererHandleFunctions(t *testing.T) {
	// Lock to main thread for OpenGL
	runtime.LockOSThread()
	defer runtime.UnlockOSThread()

	// Create renderer
	r := NewRenderer()

	// Test 1: Create context
	createData := rendererCreateContextData{
		size: geom.Size{R: 256, C: 512},
	}

	createResp, err := r.handleCreateContext(createData)
	require.NoError(t, err, "Failed to create context")

	contextID := createResp.contextID
	require.Positive(t, contextID, "Expected positive context ID")

	t.Logf("Created context with ID: %d", contextID)

	// Test 2: Compile shader
	fragmentSource := `#version 330 core
out vec4 FragColor;

uniform float t;
uniform vec4 viewport;
uniform vec3 free_camera_target;
uniform vec2 free_camera_orbit;
uniform float free_camera_zoom;

void main() {
    vec2 uv = gl_FragCoord.xy / viewport.zw;
    vec3 color = vec3(uv.x, uv.y, sin(t));
    FragColor = vec4(color, 1.0);
}`

	compileData := rendererCompileShaderData{
		contextID:      contextID,
		fragmentSource: fragmentSource,
	}

	err = r.handleCompileShader(compileData)
	require.NoError(t, err, "Failed to compile shader")

	t.Logf("Successfully compiled fragment shader")

	// Test 3: Render
	renderData := rendererRenderData{
		contextID:        contextID,
		viewportSize:     geom.Size{R: 256, C: 512}, // height=256, width=512
		time:             1.5,
		freeCameraTarget: [3]float32{0, 0, 0},
		freeCameraOrbit:  [2]float32{0.25, -0.125},
		freeCameraZoom:   1.0,
	}

	renderResp, err := r.handleRender(renderData)
	require.NoError(t, err, "Failed to render")
	require.NoError(t, renderResp.err, "Render response contained error")

	expectedPixelCount := 512 * 256 * 4 // width * height * RGBA
	require.Len(t, renderResp.pixels, expectedPixelCount, "Unexpected pixel count")

	// Check that we have non-zero pixels (indicating rendering worked)
	hasNonZero := false
	for i := 0; i < len(renderResp.pixels); i += 4 {
		if renderResp.pixels[i] != 0 || renderResp.pixels[i+1] != 0 || renderResp.pixels[i+2] != 0 {
			hasNonZero = true
			break
		}
	}
	require.True(t, hasNonZero, "All pixels are black, rendering may have failed")

	t.Logf("Successfully rendered %dx%d image with %d bytes", 512, 256, len(renderResp.pixels))

	// Test 4: Render with different viewport size (test resizing)
	renderData2 := rendererRenderData{
		contextID:        contextID,
		viewportSize:     geom.Size{R: 128, C: 256},
		time:             2.0,
		freeCameraTarget: [3]float32{1, 1, 1},
		freeCameraOrbit:  [2]float32{0.5, 0.25},
		freeCameraZoom:   2.0,
	}

	renderResp2, err := r.handleRender(renderData2)
	require.NoError(t, err, "Failed to render with new size")
	require.NoError(t, renderResp2.err, "Second render response contained error")

	expectedPixelCount2 := 256 * 128 * 4 // width * height * RGBA
	require.Len(t, renderResp2.pixels, expectedPixelCount2, "Unexpected pixel count for resized render")

	t.Logf("Successfully resized and rendered %dx%d image with %d bytes", 256, 128, len(renderResp2.pixels))

	// Test 5: Test shader compilation error
	badShaderData := rendererCompileShaderData{
		contextID:      contextID,
		fragmentSource: "invalid shader code",
	}

	err = r.handleCompileShader(badShaderData)
	require.Error(t, err, "Expected shader compilation error, but got none")

	t.Logf("Correctly caught shader compilation error: %v", err)

	// Test 6: Destroy context
	destroyData := rendererDestroyContextData{
		contextID: contextID,
	}

	err = r.handleDestroyContext(destroyData)
	require.NoError(t, err, "Failed to destroy context")

	t.Logf("Successfully destroyed context")

	// Test 7: Verify context is destroyed (should fail)
	_, err = r.getContext(contextID)
	require.Error(t, err, "Expected error when accessing destroyed context, but got none")

	t.Logf("Correctly verified context destruction: %v", err)
}
