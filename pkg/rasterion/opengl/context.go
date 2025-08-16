package opengl

import (
	"fmt"
	"runtime"
	"sync"

	"github.com/go-gl/gl/v3.3-core/gl"
	"github.com/go-gl/glfw/v3.3/glfw"
)

var (
	glfwInitialized bool
	glfwMutex       sync.Mutex
	contextCount    int
)

type Context struct {
	window *glfw.Window
	width  int
	height int
}

func NewContext(width, height int) (*Context, error) {
	runtime.LockOSThread()

	glfwMutex.Lock()
	defer glfwMutex.Unlock()

	// Initialize GLFW only once
	if !glfwInitialized {
		if err := glfw.Init(); err != nil {
			return nil, fmt.Errorf("failed to initialize GLFW: %v", err)
		}
		glfwInitialized = true
	}

	glfw.WindowHint(glfw.ContextVersionMajor, 3)
	glfw.WindowHint(glfw.ContextVersionMinor, 3)
	glfw.WindowHint(glfw.OpenGLProfile, glfw.OpenGLCoreProfile)
	glfw.WindowHint(glfw.OpenGLForwardCompatible, glfw.True)
	glfw.WindowHint(glfw.Visible, glfw.False)

	window, err := glfw.CreateWindow(width, height, "Offscreen", nil, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to create GLFW window: %v", err)
	}

	window.MakeContextCurrent()

	if err := gl.Init(); err != nil {
		window.Destroy()
		return nil, fmt.Errorf("failed to initialize OpenGL: %v", err)
	}

	contextCount++

	return &Context{
		window: window,
		width:  width,
		height: height,
	}, nil
}

func (c *Context) Destroy() {
	glfwMutex.Lock()
	defer glfwMutex.Unlock()

	if c.window != nil {
		c.window.Destroy()
		c.window = nil
		contextCount--

		// Only terminate GLFW when the last context is destroyed
		if contextCount == 0 && glfwInitialized {
			glfw.Terminate()
			glfwInitialized = false
		}
	}
}

func (c *Context) MakeCurrent() {
	c.window.MakeContextCurrent()
}

func (c *Context) Width() int {
	return c.width
}

func (c *Context) Height() int {
	return c.height
}
