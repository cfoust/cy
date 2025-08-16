package opengl

import (
	"context"
	"fmt"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
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

	// Start the rendering loop
	go screen.renderLoop()

	return screen
}

// ASCII characters for different brightness levels (dark to light)
var asciiChars = []rune{' ', '.', ':', '-', '=', '+', '*', '#', '%', '@'}

// pixelToASCII converts RGB pixel values to an ASCII character
func pixelToASCII(r, g, b uint8) rune {
	// Convert to grayscale using luminance formula
	gray := float64(r)*0.299 + float64(g)*0.587 + float64(b)*0.114

	// Map grayscale (0-255) to ASCII character index
	index := int(gray / 255.0 * float64(len(asciiChars)-1))
	if index >= len(asciiChars) {
		index = len(asciiChars) - 1
	}
	return asciiChars[index]
}

// imageToASCII converts RGBA pixel data to ASCII characters and renders to terminal
func (s *Screen) imageToASCII(pixels []byte, width, height int) {
	s.Lock()
	defer s.Unlock()

	// Clear the state
	s.state = tty.New(s.size)

	// Calculate how to fit the image into the terminal size
	scaleX := float64(width) / float64(s.size.C)
	scaleY := float64(height) / float64(s.size.R)

	for row := 0; row < s.size.R; row++ {
		for col := 0; col < s.size.C; col++ {
			// Map terminal position to pixel position
			pixelX := int(float64(col) * scaleX)
			pixelY := int(float64(row) * scaleY)

			// Ensure we're within bounds
			if pixelX >= width || pixelY >= height {
				continue
			}

			// Get pixel data (RGBA format, 4 bytes per pixel)
			pixelIndex := (pixelY*width + pixelX) * 4
			if pixelIndex+2 >= len(pixels) {
				continue
			}

			r := pixels[pixelIndex]
			g := pixels[pixelIndex+1]
			b := pixels[pixelIndex+2]

			// Convert to ASCII and set in terminal
			char := pixelToASCII(r, g, b)
			s.state.Image[row][col] = emu.Glyph{
				Char: char,
				FG:   emu.DefaultFG,
				BG:   emu.DefaultBG,
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

// renderFrame renders a single frame
func (s *Screen) renderFrame(elapsed time.Duration) {
	s.RLock()
	size := s.size
	context := s.context
	compilationErr := s.compilationErr
	s.RUnlock()

	// If we have a compilation error, show it instead of rendering
	if compilationErr != nil {
		s.showError(compilationErr)
		s.Notify()
		return
	}

	// If no context, try to create one
	if context == nil {
		var err error
		context, err = s.renderer.NewContext(s.Ctx(), size)
		if err != nil {
			s.showError(fmt.Errorf("failed to create OpenGL context: %v", err))
			s.Notify()
			return
		}

		// Compile the shader
		err = context.CompileShader(s.Ctx(), s.fragmentSource)
		if err != nil {
			s.Lock()
			s.compilationErr = err
			s.Unlock()
			s.showError(err)
			s.Notify()
			return
		}

		s.Lock()
		s.context = context
		s.Unlock()
	}

	// Render the frame
	pixels, err := context.Render(s.Ctx(), RenderParams{
		ViewportSize:     size,
		Time:             float32(elapsed.Seconds()),
		FreeCameraTarget: [3]float32{0, 0, 0},
		FreeCameraOrbit:  [2]float32{0, 0},
		FreeCameraZoom:   1.0,
	})
	if err != nil {
		s.showError(fmt.Errorf("render error: %v", err))
		s.Notify()
		return
	}

	// Convert to ASCII and update state
	s.imageToASCII(pixels, size.C, size.R)
	s.Notify()
}

// showError displays an error message centered on the screen
func (s *Screen) showError(err error) {
	s.Lock()
	defer s.Unlock()

	// Clear the state
	s.state = tty.New(s.size)

	// Create error message
	message := fmt.Sprintf("Shader Error: %s", err.Error())

	// Split message into lines that fit the screen width
	var lines []string
	maxWidth := s.size.C - 4 // Leave some padding

	for len(message) > 0 {
		if len(message) <= maxWidth {
			lines = append(lines, message)
			break
		}

		// Find a good place to break the line
		breakPoint := maxWidth
		for i := maxWidth; i > 0; i-- {
			if message[i] == ' ' {
				breakPoint = i
				break
			}
		}

		lines = append(lines, message[:breakPoint])
		message = message[breakPoint:]
		if len(message) > 0 && message[0] == ' ' {
			message = message[1:] // Skip leading space
		}
	}

	// Center the error message
	startRow := (s.size.R - len(lines)) / 2
	if startRow < 0 {
		startRow = 0
	}

	errorStyle := s.render.NewStyle().
		Foreground(lipgloss.Color("9")). // Red
		Background(lipgloss.Color("0")). // Black
		Bold(true)

	for i, line := range lines {
		row := startRow + i
		if row >= s.size.R {
			break
		}

		// Center the line horizontally
		startCol := (s.size.C - len(line)) / 2
		if startCol < 0 {
			startCol = 0
		}

		// Render the line
		s.render.RenderAt(
			s.state.Image,
			startCol,
			row,
			errorStyle.Render(line),
		)
	}
}

// State returns the current terminal state
func (s *Screen) State() *tty.State {
	s.RLock()
	defer s.RUnlock()
	return s.state.Clone()
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
