package opengl

import (
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
}

type rendererSetShaderData struct {
	contextID int
	program   *Program
}

type rendererClearData struct {
	contextID int
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

	// Main thread state
	contexts map[int]*mainThreadContext
	nextContextID int
}

// mainThreadContext holds OpenGL state for a single rendering context
type mainThreadContext struct {
	id          int
	context     *Context
	framebuffer *Framebuffer
	program     *Program
	vao         uint32
	vbo         uint32
	width       int
	height      int
}

// NewRenderer creates a new main-thread renderer
// This must be called from the main thread before calling Poll()
func NewRenderer() *Renderer {
	return &Renderer{
		commandChan: make(chan rendererCommand, 32),
		quit:        make(chan struct{}),
		contexts:    make(map[int]*mainThreadContext),
		nextContextID: 1,
	}
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
func (r *Renderer) NewContext(width, height int) *ContextHandle {
	responseChan := make(chan interface{})
	r.commandChan <- rendererCommand{
		cmd: cmdRendererCreateContext,
		data: rendererInitData{
			width:  width,
			height: height,
		},
		response: responseChan,
	}

	result := <-responseChan
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
}

func (r *Renderer) handleCommand(cmd rendererCommand) {
	switch cmd.cmd {
	case cmdRendererCreateContext:
		data := cmd.data.(rendererInitData)
		r.handleCreateContext(cmd.response, data)

	case cmdRendererSetShaderProgram:
		data := cmd.data.(rendererSetShaderData)
		r.handleSetShaderProgram(cmd.response, data)

	case cmdRendererRender:
		data := cmd.data.(rendererRenderData)
		r.handleRender(cmd.response, data)

	case cmdRendererReadPixels:
		data := cmd.data.(rendererRenderData)
		r.handleReadPixels(cmd.response, data)

	case cmdRendererClear:
		data := cmd.data.(rendererClearData)
		r.handleClear(cmd.response, data)

	case cmdRendererDestroy:
		data := cmd.data.(rendererRenderData)
		r.handleDestroyContext(cmd.response, data)
	}
}

func (r *Renderer) handleCreateContext(response chan interface{}, data rendererInitData) {
	ctx, err := NewContext(data.width, data.height)
	if err != nil {
		response <- rendererCreateContextResponse{err: err}
		return
	}

	ctx.MakeCurrent()

	fbo, err := NewFramebuffer(int32(data.width), int32(data.height))
	if err != nil {
		ctx.Destroy()
		response <- rendererCreateContextResponse{err: err}
		return
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

	r.contexts[contextID] = &mainThreadContext{
		id:          contextID,
		context:     ctx,
		framebuffer: fbo,
		vao:         vao,
		vbo:         vbo,
		width:       data.width,
		height:      data.height,
	}

	response <- rendererCreateContextResponse{contextID: contextID}
}

func (r *Renderer) handleSetShaderProgram(response chan interface{}, data rendererSetShaderData) {
	if ctx, ok := r.contexts[data.contextID]; ok {
		ctx.program = data.program
		response <- nil
	} else {
		response <- fmt.Errorf("context %d not found", data.contextID)
	}
}

func (r *Renderer) handleRender(response chan interface{}, data rendererRenderData) {
	ctx, ok := r.contexts[data.contextID]
	if !ok {
		response <- fmt.Errorf("context %d not found", data.contextID)
		return
	}

	if ctx.program == nil {
		response <- fmt.Errorf("no shader program set")
		return
	}

	ctx.context.MakeCurrent()
	ctx.framebuffer.Bind()

	ctx.program.Use()
	gl.BindVertexArray(ctx.vao)
	gl.DrawArrays(gl.TRIANGLE_FAN, 0, 4)
	gl.BindVertexArray(0)

	ctx.framebuffer.Unbind()

	response <- nil
}

func (r *Renderer) handleReadPixels(response chan interface{}, data rendererRenderData) {
	ctx, ok := r.contexts[data.contextID]
	if !ok {
		response <- rendererReadPixelsResponse{err: fmt.Errorf("context %d not found", data.contextID)}
		return
	}

	pixels, err := ctx.framebuffer.ReadPixels()
	response <- rendererReadPixelsResponse{pixels: pixels, err: err}
}

func (r *Renderer) handleClear(response chan interface{}, data rendererClearData) {
	ctx, ok := r.contexts[data.contextID]
	if !ok {
		response <- fmt.Errorf("context %d not found", data.contextID)
		return
	}

	ctx.framebuffer.Clear(data.r, data.g, data.b, data.a)
	response <- nil
}

func (r *Renderer) handleDestroyContext(response chan interface{}, data rendererRenderData) {
	if ctx, ok := r.contexts[data.contextID]; ok {
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
	}
	response <- nil
}

func (r *Renderer) cleanup() {
	for contextID := range r.contexts {
		r.handleDestroyContext(make(chan interface{}, 1), rendererRenderData{contextID: contextID})
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
func (h *ContextHandle) SetShaderProgram(program *Program) error {
	if h.err != nil {
		return h.err
	}

	responseChan := make(chan interface{})
	h.renderer.commandChan <- rendererCommand{
		cmd: cmdRendererSetShaderProgram,
		data: rendererSetShaderData{
			contextID: h.contextID,
			program:   program,
		},
		response: responseChan,
	}

	result := <-responseChan
	if err, ok := result.(error); ok {
		return err
	}
	return nil
}

// Render executes a render operation
func (h *ContextHandle) Render() error {
	if h.err != nil {
		return h.err
	}

	responseChan := make(chan interface{})
	h.renderer.commandChan <- rendererCommand{
		cmd: cmdRendererRender,
		data: rendererRenderData{
			contextID: h.contextID,
		},
		response: responseChan,
	}

	result := <-responseChan
	if err, ok := result.(error); ok {
		return err
	}
	return nil
}

// ReadPixels reads the current framebuffer contents
func (h *ContextHandle) ReadPixels() ([]byte, error) {
	if h.err != nil {
		return nil, h.err
	}

	responseChan := make(chan interface{})
	h.renderer.commandChan <- rendererCommand{
		cmd: cmdRendererReadPixels,
		data: rendererRenderData{
			contextID: h.contextID,
		},
		response: responseChan,
	}

	result := <-responseChan
	if resp, ok := result.(rendererReadPixelsResponse); ok {
		return resp.pixels, resp.err
	}
	return nil, fmt.Errorf("unexpected response type")
}

// Clear clears the framebuffer with the specified color
func (h *ContextHandle) Clear(r, g, b, a float32) error {
	if h.err != nil {
		return h.err
	}

	responseChan := make(chan interface{})
	h.renderer.commandChan <- rendererCommand{
		cmd: cmdRendererClear,
		data: rendererClearData{
			contextID: h.contextID,
			r:         r,
			g:         g,
			b:         b,
			a:         a,
		},
		response: responseChan,
	}

	result := <-responseChan
	if err, ok := result.(error); ok {
		return err
	}
	return nil
}

// Destroy destroys this context and cleans up resources
func (h *ContextHandle) Destroy() error {
	if h.err != nil {
		return h.err
	}

	responseChan := make(chan interface{})
	h.renderer.commandChan <- rendererCommand{
		cmd: cmdRendererDestroy,
		data: rendererRenderData{
			contextID: h.contextID,
		},
		response: responseChan,
	}

	result := <-responseChan
	if err, ok := result.(error); ok {
		return err
	}
	
	h.contextID = -1 // Mark as destroyed
	return nil
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
