package opengl

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/lipgloss"
	"github.com/sasha-s/go-deadlock"
)

// Screen implements mux.Screen for rendering OpenGL shaders to terminal characters
type Screen struct {
	util.Lifetime
	deadlock.RWMutex
	*mux.UpdatePublisher

	renderer       *Renderer
	fragmentSource string
	context        *ContextHandle
	compilationErr error
	lastContextErr error // Track context creation errors

	size  geom.Size
	state *tty.State

	render *taro.Renderer
}

var _ mux.Screen = (*Screen)(nil)

// New creates a new OpenGL shader screen
func NewScreen(
	ctx context.Context,
	renderer *Renderer,
	fragmentSource string,
) *Screen {
	// Use DefaultRenderer if no renderer provided
	if renderer == nil {
		renderer = DefaultRenderer
	}

	screen := &Screen{
		Lifetime:        util.NewLifetime(ctx),
		UpdatePublisher: mux.NewPublisher(),
		renderer:        renderer,
		fragmentSource:  fragmentSource,
		size:            geom.Size{R: 24, C: 80}, // Default size
		render:          taro.NewRenderer(),
	}

	screen.state = tty.New(screen.size)

	// Start the rendering loop with a small delay to let the main thread get ready
	go func() {
		time.Sleep(50 * time.Millisecond)
		screen.renderLoop()
	}()

	return screen
}

// rgbToColor converts RGB values to emu.Color using true RGB colors
func rgbToColor(r, g, b float32) emu.Color {
	return emu.RGBColor(int(r), int(g), int(b))
}

// clampFloat clamps a float32 value to the range [0,1]
func clampFloat(value float32) float32 {
	if value < 0 {
		return 0
	}
	if value > 1 {
		return 1
	}
	return value
}

// imageToTerminal converts RGBA color and glyph float data to colored terminal characters
func (s *Screen) imageToTerminal(
	colorPixels []float32,
	glyphPixels []float32,
	size geom.Size,
) {
	s.Lock()
	defer s.Unlock()

	// Clear the state
	s.state = tty.New(s.size)
	s.state.CursorVisible = false

	for row := 0; row < s.size.R; row++ {
		for col := 0; col < s.size.C; col++ {
			// Get pixel data (RGBA format, 4 floats per pixel)
			pixelIndex := (row*size.C + col) * 4
			glyphIndex := (row*size.C + col) * 3
			if pixelIndex+3 >= len(colorPixels) || glyphIndex+2 >= len(glyphPixels) {
				continue
			}

			// Extract color data (clamped to [0,1] for display)
			r := colorPixels[pixelIndex]
			g := colorPixels[pixelIndex+1]
			b := colorPixels[pixelIndex+2]

			// Extract glyph data (unclamped floats)
			glyphR := glyphPixels[glyphIndex]
			rScaled := clampFloat(r) * 255
			gScaled := clampFloat(g) * 255
			bScaled := clampFloat(b) * 255
			bgColor := rgbToColor(rScaled, gScaled, bScaled)

			// Choose character based on glyph R component
			var char rune
			if glyphR == 1.0 {
				char = 'a'
			} else {
				char = '.'
			}

			s.state.Image[row][col] = emu.Glyph{
				Char: char,
				FG:   emu.DefaultFG,
				BG:   bgColor,
			}
		}
	}
}

// renderLoop runs the 30fps rendering loop
func (s *Screen) renderLoop() {
	defer s.Cancel()

	ticker := time.NewTicker(time.Second / 30) // 30fps
	defer ticker.Stop()

	startTime := time.Now()

	for {
		select {
		case <-s.Ctx().Done():
			return
		case <-ticker.C:
			s.renderFrame(time.Since(startTime))
		}
	}
}

// tryCreateContext attempts to create and initialize an OpenGL context
// Returns true if successful, false if failed (with appropriate error display)
func (s *Screen) tryCreateContext(size geom.Size, lastContextErr error) bool {
	context, err := s.renderer.NewContext(s.Ctx(), size)
	if err != nil {
		// Only update display if this is a new error or first attempt
		if lastContextErr == nil || lastContextErr.Error() != err.Error() {
			s.Lock()
			s.lastContextErr = err
			s.Unlock()

			// Show a more user-friendly message during startup
			if lastContextErr == nil {
				s.renderMessage("initializing", "OpenGL renderer starting...", false)
			} else {
				s.renderMessage("opengl error", err.Error(), true)
			}
			s.Notify()
		}
		return false
	}

	// Successfully created context, clear any previous error
	s.Lock()
	s.lastContextErr = nil
	s.Unlock()

	// Compile the shader
	err = context.CompileShader(s.Ctx(), s.fragmentSource)
	if err != nil {
		s.Lock()
		s.compilationErr = err
		s.Unlock()
		s.renderMessage("shader error", err.Error(), true)
		s.Notify()
		return false
	}

	s.Lock()
	s.context = context
	s.Unlock()
	return true
}

// renderFrame renders a single frame
func (s *Screen) renderFrame(elapsed time.Duration) {
	s.RLock()
	size := s.size
	context := s.context
	compilationErr := s.compilationErr
	lastContextErr := s.lastContextErr
	s.RUnlock()

	// If we have a compilation error, show it instead of rendering
	if compilationErr != nil {
		s.renderMessage("shader error", compilationErr.Error(), true)
		s.Notify()
		return
	}

	// If no context, try to create one
	if context == nil {
		if !s.tryCreateContext(size, lastContextErr) {
			return
		}
		// Update context from the newly created one
		s.RLock()
		context = s.context
		s.RUnlock()
	}

	// Render the frame (now returns both color and glyph data)
	colorPixels, glyphPixels, err := context.Render(s.Ctx(), RenderParams{
		ViewportSize:     size,
		Time:             float32(elapsed.Seconds()),
		FreeCameraTarget: [3]float32{0, 0, 0},
		FreeCameraOrbit:  [2]float32{1.5, 1.5},
		FreeCameraZoom:   2.0,
	})
	if err != nil {
		s.renderMessage("render error", err.Error(), true)
		s.Notify()
		return
	}

	s.imageToTerminal(colorPixels, glyphPixels, size)
	s.Notify()
}

// renderMessage displays a message in a centered box with border
func (s *Screen) renderMessage(title, message string, isError bool) {
	s.Lock()
	defer s.Unlock()

	// Clear the state
	s.state = tty.New(s.size)
	s.state.CursorVisible = false

	size := s.state.Image.Size()
	width := geom.Min(size.C, 50)

	var borderColor lipgloss.Color
	var titleStyle lipgloss.Style

	if isError {
		borderColor = lipgloss.Color("9") // Red
		titleStyle = s.render.NewStyle().
			Foreground(borderColor).
			Bold(true)
	} else {
		borderColor = lipgloss.Color("12") // Blue
		titleStyle = s.render.NewStyle().
			Foreground(borderColor).
			Bold(true)
	}

	boxContents := lipgloss.JoinVertical(
		lipgloss.Left,
		titleStyle.Render(title),
		s.render.NewStyle().
			Width(width).
			Render(message),
	)

	boxStyle := s.render.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(borderColor).
		BorderTop(true).
		BorderLeft(true).
		BorderRight(true).
		BorderBottom(true)

	boxText := boxStyle.Render(boxContents)
	boxSize := taro.GetSize(boxText)

	box := image.New(boxSize)
	s.render.RenderAt(
		box,
		0,
		0,
		boxText,
	)
	image.Copy(size.Center(boxSize), s.state.Image, box)
}

// State returns the current terminal state
func (s *Screen) State() *tty.State {
	s.RLock()
	defer s.RUnlock()
	state := s.state.Clone()
	state.CursorVisible = false
	return state
}

// Resize resizes the screen
func (s *Screen) Resize(size mux.Size) error {
	s.Lock()
	defer s.Unlock()

	if s.size == size {
		return nil
	}

	s.size = size
	s.state = tty.New(size)

	// If we have a context, we need to recreate it with the new size
	if s.context != nil {
		_ = s.context.Destroy(context.Background())
		s.context = nil
	}

	s.Notify()
	return nil
}

// Kill stops the screen and cleans up resources
func (s *Screen) Kill() {
	if s.context != nil {
		_ = s.context.Destroy(context.Background())
	}
	s.Cancel()
}

// Send handles incoming messages (not used for this screen type)
func (s *Screen) Send(message mux.Msg) {
	// This screen doesn't handle input
}
