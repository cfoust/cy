// Package opengl provides OpenGL 3.3 shader rendering to textures that can be read from Go.
//
// This package creates an offscreen OpenGL context and provides utilities for:
// - Compiling and managing GLSL shaders
// - Creating render-to-texture framebuffers
// - Reading texture data back to Go byte slices
//
// Basic usage:
//
//	renderer, err := opengl.NewRenderer(512, 512)
//	if err != nil {
//		// handle error
//	}
//	defer renderer.Destroy()
//
//	// Compile shaders
//	vs, _ := opengl.CompileShader(vertexSource, gl.VERTEX_SHADER)
//	fs, _ := opengl.CompileShader(fragmentSource, gl.FRAGMENT_SHADER)
//	program, _ := opengl.CreateProgram(vs, fs)
//
//	renderer.SetShaderProgram(program)
//	renderer.Render()
//
//	// Read pixels as RGBA bytes
//	pixels, _ := renderer.ReadPixels()
package opengl