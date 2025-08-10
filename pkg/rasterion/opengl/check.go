package opengl

import (
	"context"
	"time"
)

// CanUseOpenGL reports whether offscreen GLFW windows can be created.
func CanUseOpenGL(ctx context.Context) bool {
	result := make(chan bool, 1)

	go func() {
		context, err := NewContext(1, 1)
		if err != nil {
			result <- false
			return
		}

		context.Destroy()

		result <- true
	}()

	select {
	case <-ctx.Done():
		return false
	case <-time.After(1 * time.Second):
		return false
	case ok := <-result:
		return ok
	}
}
