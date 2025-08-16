package opengl

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/geom"
)

// RenderParams contains all parameters needed for rendering
type RenderParams struct {
	ViewportSize     geom.Size
	Time             float32
	FreeCameraTarget [3]float32
	FreeCameraOrbit  [2]float32
	FreeCameraZoom   float32
}

// ContextHandle provides an interface for interacting with a specific OpenGL context
type ContextHandle struct {
	renderer  *Renderer
	contextID int
}

// sendCommand is a helper method that centralizes the boilerplate for sending commands
// and handling responses with proper context cancellation support
func (h *ContextHandle) sendCommand(
	ctx context.Context,
	cmd rendererCommandType,
	data interface{},
) (interface{}, error) {
	responseChan := make(chan interface{}, 1)

	select {
	case h.renderer.commandChan <- rendererCommand{
		cmd:      cmd,
		data:     data,
		response: responseChan,
	}:
	case <-ctx.Done():
		return nil, ctx.Err()
	}

	select {
	case result := <-responseChan:
		return result, nil
	case <-ctx.Done():
		return nil, ctx.Err()
	}
}

// CompileShader compiles a fragment shader and returns any compilation errors
func (h *ContextHandle) CompileShader(
	ctx context.Context,
	fragmentSource string,
) error {
	result, err := h.sendCommand(
		ctx,
		cmdRendererCompileShader,
		rendererCompileShaderData{
			contextID:      h.contextID,
			fragmentSource: fragmentSource,
		},
	)
	if err != nil {
		return err
	}

	if err, ok := result.(error); ok {
		return err
	}
	return nil
}

// Render executes a render operation with Bauble shader uniforms and returns the rendered image
func (h *ContextHandle) Render(
	ctx context.Context,
	params RenderParams,
) ([]byte, error) {
	result, err := h.sendCommand(
		ctx,
		cmdRendererRender,
		rendererRenderData{
			contextID:        h.contextID,
			viewportSize:     params.ViewportSize,
			time:             params.Time,
			freeCameraTarget: params.FreeCameraTarget,
			freeCameraOrbit:  params.FreeCameraOrbit,
			freeCameraZoom:   params.FreeCameraZoom,
		},
	)
	if err != nil {
		return nil, err
	}

	if resp, ok := result.(rendererRenderResponse); ok {
		return resp.pixels, resp.err
	}
	if err, ok := result.(error); ok {
		return nil, err
	}
	return nil, fmt.Errorf("unexpected response type")
}

// Destroy destroys this context and cleans up resources
func (h *ContextHandle) Destroy(ctx context.Context) error {
	result, err := h.sendCommand(
		ctx,
		cmdRendererDestroyContext,
		rendererDestroyContextData{
			contextID: h.contextID,
		},
	)
	if err != nil {
		return err
	}

	if err, ok := result.(error); ok {
		return err
	}
	h.contextID = -1 // Mark as destroyed
	return nil
}