package opengl

import (
	"context"
	"fmt"
	"runtime"
	"sync"

	"github.com/go-gl/gl/v3.3-core/gl"
)

type QuadVertex struct {
	X, Y float32
}

// Command types for the main-thread renderer
type rendererCommandType int

const (
	cmdRendererInit rendererCommandType = iota
	cmdRendererCreateContext
	cmdRendererSetShaderProgram
	cmdRendererRender
	cmdRendererReadPixels
	cmdRendererClear
	cmdRendererDestroy
	cmdRendererShutdown
)

type rendererCommand struct {
	cmd      rendererCommandType
	data     interface{}
	response chan interface{}
}

type rendererInitData struct {
	width  int
	height int
}

type rendererCreateContextResponse struct {
	contextID int
	err       error
}

type rendererRenderData struct {
	contextID int
	// Bauble shader uniforms
	time             float32
	viewport         [4]float32
	freeCameraTarget [3]float32
	freeCameraOrbit  [2]float32
	freeCameraZoom   float32
}

type rendererSetShaderData struct {
	contextID int
	program   *Program
}

type rendererClearData struct {
	contextID  int
	r, g, b, a float32
}

type rendererReadPixelsResponse struct {
	pixels []byte
	err    error
}

// Renderer manages OpenGL operations on the main thread
type Renderer struct {
	commandChan chan rendererCommand
	quit        chan struct{}
	running     bool
	mu          sync.RWMutex

	contexts      map[int]*renderContext
	nextContextID int
}

type renderContext struct {
	id          int
	context     *Context
	framebuffer *Framebuffer
	program     *Program
	vao         uint32
	vbo         uint32
	width       int
	height      int

	// Bauble shader uniform locations
	tLocation                int32 // time
	viewportLocation         int32 // viewport (vec4)
	freeCameraTargetLocation int32 // free_camera_target (vec3)
	freeCameraOrbitLocation  int32 // free_camera_orbit (vec2)
	freeCameraZoomLocation   int32 // free_camera_zoom (float)
}

// NewRenderer creates a new main-thread renderer
// This must be called from the main thread before calling Poll()
func NewRenderer() *Renderer {
	return &Renderer{
		commandChan:   make(chan rendererCommand, 32),
		quit:          make(chan struct{}),
		contexts:      make(map[int]*renderContext),
		nextContextID: 1,
	}
}

// getContext retrieves a render context by ID
func (r *Renderer) getContext(contextID int) (*renderContext, error) {
	if ctx, ok := r.contexts[contextID]; ok {
		return ctx, nil
	}
	return nil, fmt.Errorf("context %d not found", contextID)
}

// Poll processes OpenGL commands on the main thread
// This MUST be called continuously from the main thread
func (r *Renderer) Poll() {
	if !r.running {
		runtime.LockOSThread()
		r.running = true
	}

	for {
		select {
		case cmd := <-r.commandChan:
			r.handleCommand(cmd)
		case <-r.quit:
			r.cleanup()
			runtime.UnlockOSThread()
			return
		default:
			return // No commands to process
		}
	}
}

// Shutdown signals the renderer to stop and cleanup
func (r *Renderer) Shutdown() {
	close(r.quit)
}

// NewContext creates a new rendering context
// Returns a ContextHandle that can be used for rendering operations
func (r *Renderer) NewContext(ctx context.Context, width, height int) *ContextHandle {
	responseChan := make(chan interface{})

	select {
	case r.commandChan <- rendererCommand{
		cmd: cmdRendererCreateContext,
		data: rendererInitData{
			width:  width,
			height: height,
		},
		response: responseChan,
	}:
	case <-ctx.Done():
		return &ContextHandle{renderer: r, contextID: -1, err: ctx.Err()}
	}

	select {
	case result := <-responseChan:
		if resp, ok := result.(rendererCreateContextResponse); ok {
			if resp.err != nil {
				return &ContextHandle{renderer: r, contextID: -1, err: resp.err}
			}
			return &ContextHandle{
				renderer:  r,
				contextID: resp.contextID,
			}
		}
		return &ContextHandle{renderer: r, contextID: -1, err: fmt.Errorf("unexpected response type")}
	case <-ctx.Done():
		return &ContextHandle{renderer: r, contextID: -1, err: ctx.Err()}
	}
}

func (r *Renderer) handleCommand(cmd rendererCommand) {
	var result interface{}
	var err error

	switch cmd.cmd {
	case cmdRendererCreateContext:
		data := cmd.data.(rendererInitData)
		result, err = r.handleCreateContext(data)

	case cmdRendererSetShaderProgram:
		data := cmd.data.(rendererSetShaderData)
		err = r.handleSetShaderProgram(data)

	case cmdRendererRender:
		data := cmd.data.(rendererRenderData)
		err = r.handleRender(data)

	case cmdRendererReadPixels:
		data := cmd.data.(rendererRenderData)
		result, err = r.handleReadPixels(data)

	case cmdRendererClear:
		data := cmd.data.(rendererClearData)
		err = r.handleClear(data)

	case cmdRendererDestroy:
		data := cmd.data.(rendererRenderData)
		err = r.handleDestroyContext(data)
	}

	// Send result or error back through response channel
	if err != nil {
		cmd.response <- err
	} else if result != nil {
		cmd.response <- result
	} else {
		cmd.response <- nil
	}
}

func (r *Renderer) handleCreateContext(data rendererInitData) (rendererCreateContextResponse, error) {
	ctx, err := NewContext(data.width, data.height)
	if err != nil {
		return rendererCreateContextResponse{}, err
	}

	ctx.MakeCurrent()

	fbo, err := NewFramebuffer(int32(data.width), int32(data.height))
	if err != nil {
		ctx.Destroy()
		return rendererCreateContextResponse{}, err
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

	contextID := r.nextContextID
	r.nextContextID++

	r.contexts[contextID] = &renderContext{
		id:          contextID,
		context:     ctx,
		framebuffer: fbo,
		vao:         vao,
		vbo:         vbo,
		width:       data.width,
		height:      data.height,
	}

	return rendererCreateContextResponse{contextID: contextID}, nil
}

func (r *Renderer) handleSetShaderProgram(data rendererSetShaderData) error {
	ctx, err := r.getContext(data.contextID)
	if err != nil {
		return err
	}

	ctx.program = data.program

	// Get Bauble shader uniform locations
	if data.program != nil {
		ctx.tLocation = data.program.GetUniformLocation("t")
		ctx.viewportLocation = data.program.GetUniformLocation("viewport")
		ctx.freeCameraTargetLocation = data.program.GetUniformLocation("free_camera_target")
		ctx.freeCameraOrbitLocation = data.program.GetUniformLocation("free_camera_orbit")
		ctx.freeCameraZoomLocation = data.program.GetUniformLocation("free_camera_zoom")
	}

	return nil
}

func (r *Renderer) handleRender(data rendererRenderData) error {
	ctx, err := r.getContext(data.contextID)
	if err != nil {
		return err
	}

	if ctx.program == nil {
		return fmt.Errorf("no shader program set")
	}

	ctx.context.MakeCurrent()
	ctx.framebuffer.Bind()

	ctx.program.Use()

	// Set Bauble shader uniforms
	if ctx.tLocation >= 0 {
		ctx.program.SetUniform1f(ctx.tLocation, data.time)
	}
	if ctx.viewportLocation >= 0 {
		ctx.program.SetUniform4f(ctx.viewportLocation, data.viewport[0], data.viewport[1], data.viewport[2], data.viewport[3])
	}
	if ctx.freeCameraTargetLocation >= 0 {
		ctx.program.SetUniform3f(ctx.freeCameraTargetLocation, data.freeCameraTarget[0], data.freeCameraTarget[1], data.freeCameraTarget[2])
	}
	if ctx.freeCameraOrbitLocation >= 0 {
		ctx.program.SetUniform2f(ctx.freeCameraOrbitLocation, data.freeCameraOrbit[0], data.freeCameraOrbit[1])
	}
	if ctx.freeCameraZoomLocation >= 0 {
		ctx.program.SetUniform1f(ctx.freeCameraZoomLocation, data.freeCameraZoom)
	}

	gl.BindVertexArray(ctx.vao)
	gl.DrawArrays(gl.TRIANGLE_FAN, 0, 4)
	gl.BindVertexArray(0)

	ctx.framebuffer.Unbind()

	return nil
}

func (r *Renderer) handleReadPixels(data rendererRenderData) (rendererReadPixelsResponse, error) {
	ctx, err := r.getContext(data.contextID)
	if err != nil {
		return rendererReadPixelsResponse{}, err
	}

	pixels, err := ctx.framebuffer.ReadPixels()
	return rendererReadPixelsResponse{pixels: pixels, err: err}, nil
}

func (r *Renderer) handleClear(data rendererClearData) error {
	ctx, err := r.getContext(data.contextID)
	if err != nil {
		return err
	}

	ctx.framebuffer.Clear(data.r, data.g, data.b, data.a)
	return nil
}

func (r *Renderer) handleDestroyContext(data rendererRenderData) error {
	ctx, err := r.getContext(data.contextID)
	if err != nil {
		return err
	}

	if ctx.vao != 0 {
		gl.DeleteVertexArrays(1, &ctx.vao)
	}
	if ctx.vbo != 0 {
		gl.DeleteBuffers(1, &ctx.vbo)
	}
	if ctx.framebuffer != nil {
		ctx.framebuffer.Delete()
	}
	if ctx.context != nil {
		ctx.context.Destroy()
	}
	delete(r.contexts, data.contextID)
	return nil
}

func (r *Renderer) cleanup() {
	for contextID := range r.contexts {
		r.handleDestroyContext(rendererRenderData{contextID: contextID})
	}
}

// ContextHandle provides an interface for interacting with a specific OpenGL context
type ContextHandle struct {
	renderer  *Renderer
	contextID int
	err       error
}

// Error returns any error from context creation
func (h *ContextHandle) Error() error {
	return h.err
}

// SetShaderProgram sets the shader program for this context
func (h *ContextHandle) SetShaderProgram(ctx context.Context, program *Program) error {
	if h.err != nil {
		return h.err
	}

	responseChan := make(chan interface{})

	select {
	case h.renderer.commandChan <- rendererCommand{
		cmd: cmdRendererSetShaderProgram,
		data: rendererSetShaderData{
			contextID: h.contextID,
			program:   program,
		},
		response: responseChan,
	}:
	case <-ctx.Done():
		return ctx.Err()
	}

	select {
	case result := <-responseChan:
		if err, ok := result.(error); ok {
			return err
		}
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

// Render executes a render operation with Bauble shader uniforms
func (h *ContextHandle) Render(ctx context.Context, time float32, viewport [4]float32, freeCameraTarget [3]float32, freeCameraOrbit [2]float32, freeCameraZoom float32) error {
	if h.err != nil {
		return h.err
	}

	responseChan := make(chan interface{})

	select {
	case h.renderer.commandChan <- rendererCommand{
		cmd: cmdRendererRender,
		data: rendererRenderData{
			contextID:        h.contextID,
			time:             time,
			viewport:         viewport,
			freeCameraTarget: freeCameraTarget,
			freeCameraOrbit:  freeCameraOrbit,
			freeCameraZoom:   freeCameraZoom,
		},
		response: responseChan,
	}:
	case <-ctx.Done():
		return ctx.Err()
	}

	select {
	case result := <-responseChan:
		if err, ok := result.(error); ok {
			return err
		}
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

// ReadPixels reads the current framebuffer contents
func (h *ContextHandle) ReadPixels(ctx context.Context) ([]byte, error) {
	if h.err != nil {
		return nil, h.err
	}

	responseChan := make(chan interface{})

	select {
	case h.renderer.commandChan <- rendererCommand{
		cmd: cmdRendererReadPixels,
		data: rendererRenderData{
			contextID: h.contextID,
		},
		response: responseChan,
	}:
	case <-ctx.Done():
		return nil, ctx.Err()
	}

	select {
	case result := <-responseChan:
		if resp, ok := result.(rendererReadPixelsResponse); ok {
			return resp.pixels, resp.err
		}
		return nil, fmt.Errorf("unexpected response type")
	case <-ctx.Done():
		return nil, ctx.Err()
	}
}

// Clear clears the framebuffer with the specified color
func (h *ContextHandle) Clear(ctx context.Context, r, g, b, a float32) error {
	if h.err != nil {
		return h.err
	}

	responseChan := make(chan interface{})

	select {
	case h.renderer.commandChan <- rendererCommand{
		cmd: cmdRendererClear,
		data: rendererClearData{
			contextID: h.contextID,
			r:         r,
			g:         g,
			b:         b,
			a:         a,
		},
		response: responseChan,
	}:
	case <-ctx.Done():
		return ctx.Err()
	}

	select {
	case result := <-responseChan:
		if err, ok := result.(error); ok {
			return err
		}
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

// Destroy destroys this context and cleans up resources
func (h *ContextHandle) Destroy(ctx context.Context) error {
	if h.err != nil {
		return h.err
	}

	responseChan := make(chan interface{})

	select {
	case h.renderer.commandChan <- rendererCommand{
		cmd: cmdRendererDestroy,
		data: rendererRenderData{
			contextID: h.contextID,
		},
		response: responseChan,
	}:
	case <-ctx.Done():
		return ctx.Err()
	}

	select {
	case result := <-responseChan:
		if err, ok := result.(error); ok {
			return err
		}
		h.contextID = -1 // Mark as destroyed
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

// Width returns the framebuffer width
func (h *ContextHandle) Width() int {
	if h.err != nil || h.contextID == -1 {
		return 0
	}

	h.renderer.mu.RLock()
	defer h.renderer.mu.RUnlock()

	if ctx, ok := h.renderer.contexts[h.contextID]; ok {
		return ctx.width
	}
	return 0
}

// Height returns the framebuffer height
func (h *ContextHandle) Height() int {
	if h.err != nil || h.contextID == -1 {
		return 0
	}

	h.renderer.mu.RLock()
	defer h.renderer.mu.RUnlock()

	if ctx, ok := h.renderer.contexts[h.contextID]; ok {
		return ctx.height
	}
	return 0
}
