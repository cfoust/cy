package opengl

import (
	"context"
	"fmt"
	"runtime"
	"sync"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/go-gl/gl/v3.3-core/gl"
)

type QuadVertex struct {
	X, Y float32
}

// Command types for the main-thread renderer
type rendererCommandType int

const (
	cmdRendererCreateContext rendererCommandType = iota
	cmdRendererCompileShader
	cmdRendererRender
	cmdRendererDestroyContext
)

type rendererCommand struct {
	cmd      rendererCommandType
	data     interface{}
	response chan interface{}
}

type rendererCreateContextData struct {
	width  int
	height int
}

type rendererCreateContextResponse struct {
	contextID int
	err       error
}

type rendererCompileShaderData struct {
	contextID      int
	fragmentSource string
}

type rendererRenderData struct {
	contextID        int
	viewportSize     geom.Size
	time             float32
	freeCameraTarget [3]float32
	freeCameraOrbit  [2]float32
	freeCameraZoom   float32
}

type rendererRenderResponse struct {
	pixels []byte
	err    error
}

type rendererDestroyContextData struct {
	contextID int
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
	vao         uint32
	vbo         uint32
	width       int
	height      int

	// Bauble's constant vertex shader and current program
	vertexShader   *Shader
	fragmentShader *Shader
	program        *Program

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
		data: rendererCreateContextData{
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
		data := cmd.data.(rendererCreateContextData)
		result, err = r.handleCreateContext(data)

	case cmdRendererCompileShader:
		data := cmd.data.(rendererCompileShaderData)
		err = r.handleCompileShader(data)

	case cmdRendererRender:
		data := cmd.data.(rendererRenderData)
		result, err = r.handleRender(data)

	case cmdRendererDestroyContext:
		data := cmd.data.(rendererDestroyContextData)
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

const vertexSource = "#version 300 es\nin vec4 position;void main(){gl_Position=position;}"

func (r *Renderer) handleCreateContext(data rendererCreateContextData) (rendererCreateContextResponse, error) {
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

	vertexShader, err := CompileShader(vertexSource+"\x00", gl.VERTEX_SHADER)
	if err != nil {
		fbo.Delete()
		ctx.Destroy()
		return rendererCreateContextResponse{}, fmt.Errorf("failed to compile Bauble vertex shader: %w", err)
	}

	// Create a fullscreen quad with triangles (matching Bauble's player.ts)
	left := -float32(data.width) * 0.5
	right := float32(data.width) * 0.5
	top := float32(data.height) * 0.5
	bottom := -float32(data.height) * 0.5

	vertices := []float32{
		left, top, 0, // Triangle 1
		right, top, 0,
		right, bottom, 0,
		right, bottom, 0, // Triangle 2
		left, top, 0,
		left, bottom, 0,
	}

	var vao, vbo uint32
	gl.GenVertexArrays(1, &vao)
	gl.GenBuffers(1, &vbo)

	gl.BindVertexArray(vao)
	gl.BindBuffer(gl.ARRAY_BUFFER, vbo)
	gl.BufferData(
		gl.ARRAY_BUFFER,
		len(vertices)*4, // 4 bytes per float32
		gl.Ptr(vertices),
		gl.STATIC_DRAW,
	)

	gl.VertexAttribPointerWithOffset(0, 3, gl.FLOAT, false, 12, 0) // position has 3 components
	gl.EnableVertexAttribArray(0)

	gl.BindVertexArray(0)
	gl.BindBuffer(gl.ARRAY_BUFFER, 0)

	contextID := r.nextContextID
	r.nextContextID++

	r.contexts[contextID] = &renderContext{
		id:           contextID,
		context:      ctx,
		framebuffer:  fbo,
		vao:          vao,
		vbo:          vbo,
		width:        data.width,
		height:       data.height,
		vertexShader: vertexShader,
	}

	return rendererCreateContextResponse{contextID: contextID}, nil
}

func (r *Renderer) handleCompileShader(data rendererCompileShaderData) error {
	ctx, err := r.getContext(data.contextID)
	if err != nil {
		return err
	}

	// Clean up old fragment shader and program
	if ctx.program != nil {
		ctx.program.Delete()
		ctx.program = nil
	}
	if ctx.fragmentShader != nil {
		ctx.fragmentShader.Delete()
		ctx.fragmentShader = nil
	}

	// Compile new fragment shader
	fragmentShader, err := CompileShader(data.fragmentSource+"\x00", gl.FRAGMENT_SHADER)
	if err != nil {
		return err // Return compilation error directly
	}

	// Create new program with constant vertex shader and new fragment shader
	program, err := CreateProgram(ctx.vertexShader, fragmentShader)
	if err != nil {
		fragmentShader.Delete()
		return err
	}

	ctx.fragmentShader = fragmentShader
	ctx.program = program

	// Get Bauble shader uniform locations
	ctx.tLocation = program.GetUniformLocation("t")
	ctx.viewportLocation = program.GetUniformLocation("viewport")
	ctx.freeCameraTargetLocation = program.GetUniformLocation("free_camera_target")
	ctx.freeCameraOrbitLocation = program.GetUniformLocation("free_camera_orbit")
	ctx.freeCameraZoomLocation = program.GetUniformLocation("free_camera_zoom")

	return nil
}

func (r *Renderer) handleRender(data rendererRenderData) (rendererRenderResponse, error) {
	ctx, err := r.getContext(data.contextID)
	if err != nil {
		return rendererRenderResponse{}, err
	}

	if ctx.program == nil {
		return rendererRenderResponse{}, fmt.Errorf("no shader program compiled")
	}

	// Check if viewport size has changed and resize framebuffer if needed
	viewportWidth := data.viewportSize.C
	viewportHeight := data.viewportSize.R
	if ctx.width != viewportWidth || ctx.height != viewportHeight {
		// Delete old framebuffer and create new one with new size
		if ctx.framebuffer != nil {
			ctx.framebuffer.Delete()
		}

		fbo, err := NewFramebuffer(int32(viewportWidth), int32(viewportHeight))
		if err != nil {
			return rendererRenderResponse{}, fmt.Errorf("failed to resize framebuffer: %w", err)
		}

		ctx.framebuffer = fbo
		ctx.width = viewportWidth
		ctx.height = viewportHeight
	}

	ctx.context.MakeCurrent()
	ctx.framebuffer.Bind()

	ctx.program.Use()

	// Set Bauble shader uniforms
	if ctx.tLocation >= 0 {
		ctx.program.SetUniform1f(ctx.tLocation, data.time)
	}
	if ctx.viewportLocation >= 0 {
		// Set viewport uniform as (x, y, width, height) - using 0,0 as origin
		ctx.program.SetUniform4f(ctx.viewportLocation, 0, 0, float32(viewportWidth), float32(viewportHeight))
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
	gl.DrawArrays(gl.TRIANGLES, 0, 6) // 6 vertices for 2 triangles
	gl.BindVertexArray(0)

	ctx.framebuffer.Unbind()

	// Read pixels and return them
	pixels, err := ctx.framebuffer.ReadPixels()
	return rendererRenderResponse{pixels: pixels, err: err}, nil
}

func (r *Renderer) handleDestroyContext(data rendererDestroyContextData) error {
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
	if ctx.program != nil {
		ctx.program.Delete()
	}
	if ctx.fragmentShader != nil {
		ctx.fragmentShader.Delete()
	}
	if ctx.vertexShader != nil {
		ctx.vertexShader.Delete()
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
		r.handleDestroyContext(rendererDestroyContextData{contextID: contextID})
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

// CompileShader compiles a fragment shader and returns any compilation errors
func (h *ContextHandle) CompileShader(ctx context.Context, fragmentSource string) error {
	if h.err != nil {
		return h.err
	}

	responseChan := make(chan interface{})

	select {
	case h.renderer.commandChan <- rendererCommand{
		cmd: cmdRendererCompileShader,
		data: rendererCompileShaderData{
			contextID:      h.contextID,
			fragmentSource: fragmentSource,
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

// Render executes a render operation with Bauble shader uniforms and returns the rendered image
func (h *ContextHandle) Render(ctx context.Context, viewportSize geom.Size, time float32, freeCameraTarget [3]float32, freeCameraOrbit [2]float32, freeCameraZoom float32) ([]byte, error) {
	if h.err != nil {
		return nil, h.err
	}

	responseChan := make(chan interface{})

	select {
	case h.renderer.commandChan <- rendererCommand{
		cmd: cmdRendererRender,
		data: rendererRenderData{
			contextID:        h.contextID,
			viewportSize:     viewportSize,
			time:             time,
			freeCameraTarget: freeCameraTarget,
			freeCameraOrbit:  freeCameraOrbit,
			freeCameraZoom:   freeCameraZoom,
		},
		response: responseChan,
	}:
	case <-ctx.Done():
		return nil, ctx.Err()
	}

	select {
	case result := <-responseChan:
		if resp, ok := result.(rendererRenderResponse); ok {
			return resp.pixels, resp.err
		}
		if err, ok := result.(error); ok {
			return nil, err
		}
		return nil, fmt.Errorf("unexpected response type")
	case <-ctx.Done():
		return nil, ctx.Err()
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
		cmd: cmdRendererDestroyContext,
		data: rendererDestroyContextData{
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
