package opengl

import (
	"context"
	"fmt"
	"runtime"
	"sync"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/events"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"

	"github.com/go-gl/gl/v3.3-core/gl"
	"github.com/rs/zerolog/log"
)

// Command types for communication with the rendering goroutine
type commandType int

const (
	cmdInit commandType = iota
	cmdRender
	cmdResize
	cmdSetTime
	cmdSetFrame
	cmdKill
)

type command struct {
	cmd      commandType
	data     interface{}
	response chan interface{}
}

type initData struct {
	fragmentSource string
	width          int
	height         int
}

type resizeData struct {
	width  int
	height int
}

type renderResponse struct {
	state *tty.State
	err   error
}

// Screen implements mux.Screen and renders a fragment shader to generate terminal output
type Screen struct {
	fragmentSource  string
	width           int
	height          int
	publisher       *mux.UpdatePublisher
	currentState    *tty.State
	killed          bool
	mu              sync.RWMutex

	// Communication with rendering goroutine
	commandChan chan command
	doneChan    chan struct{}

	// Animation state
	startTime float64
	frame     int32
}

// renderingContext holds the OpenGL state that must be accessed only from the rendering goroutine
type renderingContext struct {
	renderer        *Renderer
	program         *Program
	width           int
	height          int

	// Shader uniforms
	timeLocation       int32
	resolutionLocation int32
	frameLocation      int32
}

// NewScreen creates a new OpenGL shader screen
func NewScreen(fragmentSource string, width, height int) (*Screen, error) {
	screen := &Screen{
		fragmentSource: fragmentSource,
		width:          width,
		height:         height,
		publisher:      mux.NewPublisher(),
		commandChan:    make(chan command),
		doneChan:       make(chan struct{}),
		startTime:      0,
		frame:          0,
	}

	// Create initial state
	screen.currentState = tty.New(geom.Vec2{R: height, C: width})

	// Start the rendering goroutine
	go screen.renderingLoop()

	// Initialize the OpenGL context in the rendering goroutine
	responseChan := make(chan interface{})
	screen.commandChan <- command{
		cmd: cmdInit,
		data: initData{
			fragmentSource: fragmentSource,
			width:          width,
			height:         height,
		},
		response: responseChan,
	}

	// Wait for initialization to complete
	result := <-responseChan
	if err, ok := result.(error); ok {
		screen.Kill()
		return nil, err
	}

	return screen, nil
}

// Resize implements mux.Resizable
func (s *Screen) Resize(size mux.Size) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	if s.killed {
		return fmt.Errorf("screen has been killed")
	}

	s.width = size.C
	s.height = size.R

	// Update current state size
	s.currentState = tty.New(geom.Vec2{R: size.R, C: size.C})

	// Send resize command to rendering goroutine
	responseChan := make(chan interface{})
	s.commandChan <- command{
		cmd: cmdResize,
		data: resizeData{
			width:  size.C,
			height: size.R,
		},
		response: responseChan,
	}

	// Wait for resize to complete
	result := <-responseChan
	if err, ok := result.(error); ok {
		return err
	}

	s.publisher.Notify()
	return nil
}

// Kill implements mux.Killable
func (s *Screen) Kill() {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	if s.killed {
		return
	}
	
	s.killed = true

	// Send kill command to rendering goroutine
	responseChan := make(chan interface{})
	s.commandChan <- command{
		cmd:      cmdKill,
		response: responseChan,
	}

	// Wait for kill to complete
	<-responseChan
	
	// Wait for rendering goroutine to exit
	<-s.doneChan
	
	s.publisher.Notify()
}

// State implements mux.Screen - renders the shader and converts pixels to glyphs
func (s *Screen) State() *tty.State {
	s.mu.RLock()
	if s.killed {
		state := s.currentState
		s.mu.RUnlock()
		return state
	}
	s.mu.RUnlock()

	// Send render command to rendering goroutine
	responseChan := make(chan interface{})
	s.commandChan <- command{
		cmd:      cmdRender,
		response: responseChan,
	}

	// Wait for render to complete
	result := <-responseChan
	if resp, ok := result.(renderResponse); ok {
		if resp.err == nil && resp.state != nil {
			s.mu.Lock()
			s.currentState = resp.state
			// Update animation state
			s.startTime += 0.016 // ~60 FPS
			s.frame++
			s.mu.Unlock()
		}
		s.mu.RLock()
		state := s.currentState
		s.mu.RUnlock()
		return state
	}

	s.mu.RLock()
	state := s.currentState
	s.mu.RUnlock()
	return state
}

// Subscribe implements mux.Screen
func (s *Screen) Subscribe(ctx context.Context) *mux.Updater {
	return s.publisher.Subscribe(ctx)
}

// Send implements mux.Screen
func (s *Screen) Send(message events.Msg) {
	// This screen doesn't handle input messages
}

// SetTime updates the shader time uniform
func (s *Screen) SetTime(time float64) {
	s.mu.Lock()
	s.startTime = time
	s.mu.Unlock()
	
	responseChan := make(chan interface{})
	s.commandChan <- command{
		cmd:      cmdSetTime,
		data:     time,
		response: responseChan,
	}
	<-responseChan // Wait for completion
	
	s.publisher.Notify()
}

// SetFrame updates the shader frame uniform
func (s *Screen) SetFrame(frame int32) {
	s.mu.Lock()
	s.frame = frame
	s.mu.Unlock()
	
	responseChan := make(chan interface{})
	s.commandChan <- command{
		cmd:      cmdSetFrame,
		data:     frame,
		response: responseChan,
	}
	<-responseChan // Wait for completion
	
	s.publisher.Notify()
}

// GetFrameSize returns the OpenGL framebuffer dimensions
func (s *Screen) GetFrameSize() (int, int) {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	// For now, return the renderer size which should match width/height
	// In the future, this could be different if we support different framebuffer sizes
	return s.width, s.height
}

// renderingLoop runs in a dedicated goroutine with locked OS thread for OpenGL operations
func (s *Screen) renderingLoop() {
	// Lock this goroutine to the OS thread for OpenGL context
	runtime.LockOSThread()
	defer runtime.UnlockOSThread()
	defer close(s.doneChan)

	// This goroutine needs exclusive access to the OpenGL context
	// All OpenGL operations must happen here
	var ctx *renderingContext

	for cmd := range s.commandChan {
		switch cmd.cmd {
		case cmdInit:
			data := cmd.data.(initData)
			err := s.initOpenGL(&ctx, data)
			cmd.response <- err

		case cmdRender:
			state, err := s.renderFrame(ctx)
			cmd.response <- renderResponse{state: state, err: err}

		case cmdResize:
			data := cmd.data.(resizeData)
			err := s.resizeOpenGL(ctx, data)
			cmd.response <- err

		case cmdSetTime:
			// Time is handled in the main thread, no OpenGL operation needed
			cmd.response <- nil

		case cmdSetFrame:
			// Frame is handled in the main thread, no OpenGL operation needed
			cmd.response <- nil

		case cmdKill:
			s.cleanupOpenGL(ctx)
			cmd.response <- nil
			return
		}
	}
}

// initOpenGL initializes the OpenGL context and shaders (runs in rendering goroutine)
func (s *Screen) initOpenGL(ctx **renderingContext, data initData) error {
	log.Info().Msgf("initOpenGL")
	renderer, err := NewRenderer(data.width, data.height)
	if err != nil {
		return fmt.Errorf("failed to create renderer: %w", err)
	}
	log.Info().Msgf("initOpenGL b")

	// Create shader program
	vertexSource := `#version 330 core
layout (location = 0) in vec2 aPos;
void main() {
    gl_Position = vec4(aPos, 0.0, 1.0);
}`

	vertexShader, err := CompileShader(vertexSource, gl.VERTEX_SHADER)
	if err != nil {
		renderer.Destroy()
		return fmt.Errorf("failed to compile vertex shader: %w", err)
	}
	defer vertexShader.Delete()

	fragShader, err := CompileShader(data.fragmentSource, gl.FRAGMENT_SHADER)
	if err != nil {
		renderer.Destroy()
		return fmt.Errorf("failed to compile fragment shader: %w", err)
	}
	defer fragShader.Delete()

	program, err := CreateProgram(vertexShader, fragShader)
	if err != nil {
		renderer.Destroy()
		return fmt.Errorf("failed to create shader program: %w", err)
	}

	renderer.SetShaderProgram(program)

	// Get uniform locations
	timeLocation := program.GetUniformLocation("iTime")
	resolutionLocation := program.GetUniformLocation("iResolution")
	frameLocation := program.GetUniformLocation("iFrame")

	*ctx = &renderingContext{
		renderer:           renderer,
		program:            program,
		width:              data.width,
		height:             data.height,
		timeLocation:       timeLocation,
		resolutionLocation: resolutionLocation,
		frameLocation:      frameLocation,
	}

	return nil
}

// renderFrame renders the shader and returns the terminal state (runs in rendering goroutine)
func (s *Screen) renderFrame(ctx *renderingContext) (*tty.State, error) {
	if ctx == nil || ctx.renderer == nil {
		return nil, fmt.Errorf("rendering context not initialized")
	}

	s.mu.RLock()
	startTime := s.startTime
	frame := s.frame
	termWidth := s.width
	termHeight := s.height
	s.mu.RUnlock()

	// Set shader uniforms
	if ctx.timeLocation >= 0 {
		ctx.program.SetUniform1f(ctx.timeLocation, float32(startTime))
	}
	if ctx.resolutionLocation >= 0 {
		ctx.program.SetUniform2f(ctx.resolutionLocation, float32(ctx.renderer.Width()), float32(ctx.renderer.Height()))
	}
	if ctx.frameLocation >= 0 {
		ctx.program.SetUniform1i(ctx.frameLocation, frame)
	}

	// Render the shader
	err := ctx.renderer.Render()
	if err != nil {
		return nil, err
	}

	// Read pixels from framebuffer
	pixels, err := ctx.renderer.ReadPixels()
	if err != nil {
		return nil, err
	}

	// Convert pixels to glyphs
	state := s.pixelsToState(pixels, ctx.renderer.Width(), ctx.renderer.Height(), termWidth, termHeight)
	return state, nil
}

// resizeOpenGL handles resize operations (runs in rendering goroutine)
func (s *Screen) resizeOpenGL(ctx *renderingContext, data resizeData) error {
	if ctx == nil {
		return fmt.Errorf("rendering context not initialized")
	}
	
	// For now, we don't resize the OpenGL framebuffer
	// We just update the terminal dimensions and let pixelsToState handle the mapping
	ctx.width = data.width
	ctx.height = data.height
	
	return nil
}

// cleanupOpenGL cleans up OpenGL resources (runs in rendering goroutine)
func (s *Screen) cleanupOpenGL(ctx *renderingContext) {
	if ctx != nil && ctx.renderer != nil {
		ctx.renderer.Destroy()
	}
}

// pixelsToState converts RGBA pixel data to terminal glyphs
func (s *Screen) pixelsToState(pixels []byte, renderWidth, renderHeight, termWidth, termHeight int) *tty.State {
	if len(pixels) == 0 || termWidth <= 0 || termHeight <= 0 {
		return tty.New(geom.Vec2{R: termHeight, C: termWidth})
	}

	// Calculate pixel-to-character mapping
	pixelsPerCharX := float64(renderWidth) / float64(termWidth)
	pixelsPerCharY := float64(renderHeight) / float64(termHeight)

	// Create new image
	img := image.New(geom.Vec2{R: termHeight, C: termWidth})

	// Map pixels to characters
	for row := 0; row < termHeight; row++ {
		for col := 0; col < termWidth; col++ {
			// Sample pixel at the center of the character cell
			pixelX := int(float64(col) * pixelsPerCharX + pixelsPerCharX/2)
			pixelY := int(float64(row) * pixelsPerCharY + pixelsPerCharY/2)
			
			// Clamp to valid pixel coordinates
			pixelX = clamp(pixelX, 0, renderWidth-1)
			pixelY = clamp(pixelY, 0, renderHeight-1)
			
			// OpenGL framebuffer is bottom-up, so flip Y coordinate
			pixelY = renderHeight - 1 - pixelY
			
			// Get RGBA values
			pixelIndex := (pixelY*renderWidth + pixelX) * 4
			if pixelIndex+3 < len(pixels) {
				r := pixels[pixelIndex]
				g := pixels[pixelIndex+1]
				b := pixels[pixelIndex+2]
				// a := pixels[pixelIndex+3] // alpha not used for now
				
				// Convert RGB to glyph
				glyph := rgbToGlyph(r, g, b)
				img[row][col] = glyph
			}
		}
	}

	// Return new state
	return &tty.State{
		Image:         img,
		CursorVisible: false, // No cursor for shader rendering
	}
}

// rgbToGlyph converts RGB color values to a terminal glyph
func rgbToGlyph(r, g, b byte) emu.Glyph {
	// Calculate brightness
	brightness := (float64(r)*0.299 + float64(g)*0.587 + float64(b)*0.114) / 255.0
	
	// Map brightness to characters (from darkest to brightest)
	chars := []rune{' ', '░', '▒', '▓', '█'}
	charIndex := int(brightness * float64(len(chars)-1))
	if charIndex >= len(chars) {
		charIndex = len(chars) - 1
	}
	
	char := chars[charIndex]
	
	// Create color from RGB
	fg := emu.RGBColor(int(r), int(g), int(b))
	
	// Use darker background for contrast
	bg := emu.RGBColor(int(r)/4, int(g)/4, int(b)/4)
	
	return emu.Glyph{
		Char: char,
		FG:   fg,
		BG:   bg,
		Mode: 0,
	}
}

// clamp ensures value is within [min, max] range
func clamp(value, min, max int) int {
	if value < min {
		return min
	}
	if value > max {
		return max
	}
	return value
}
