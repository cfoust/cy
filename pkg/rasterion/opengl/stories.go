package opengl

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/stories"
)

func init() {
	// OpenGL shader stories
	stories.Register(
		"opengl/basic-colors",
		func(ctx context.Context) (mux.Screen, error) {
			fragmentSource := `#version 330 core
out vec4 frag_color;

uniform float t;
uniform vec4 viewport;

void main() {
    vec2 uv = gl_FragCoord.xy / viewport.zw;
    vec3 color = vec3(uv.x, uv.y, sin(t));
    frag_color = vec4(color, 1.0);
}`
			return NewScreen(ctx, nil, fragmentSource), nil
		},
		stories.Config{
			Size: geom.Size{R: 20, C: 40},
		},
	)

	stories.Register(
		"opengl/animated-gradient",
		func(ctx context.Context) (mux.Screen, error) {
			fragmentSource := `#version 330 core
out vec4 frag_color;

uniform float t;
uniform vec4 viewport;

void main() {
    vec2 uv = gl_FragCoord.xy / viewport.zw;
    vec3 color = 0.5 + 0.5 * cos(t + uv.xyx + vec3(0, 2, 4));
    frag_color = vec4(color, 1.0);
}`
			return NewScreen(ctx, nil, fragmentSource), nil
		},
		stories.Config{
			Size: geom.Size{R: 24, C: 60},
		},
	)

	stories.Register(
		"opengl/plasma",
		func(ctx context.Context) (mux.Screen, error) {
			fragmentSource := `#version 330 core
out vec4 frag_color;

uniform float t;
uniform vec4 viewport;

void main() {
    vec2 uv = (gl_FragCoord.xy - 0.5 * viewport.zw) / min(viewport.z, viewport.w);
    
    float v = 0.0;
    v += sin((uv.x + t));
    v += sin((uv.y + t) / 2.0);
    v += sin((uv.x + uv.y + t) / 2.0);
    
    vec2 c = uv + vec2(sin(t / 3.0), cos(t / 2.0));
    v += sin(sqrt(c.x * c.x + c.y * c.y + 1.0) + t);
    v = v / 2.0;
    
    vec3 color = vec3(sin(v * 3.14159), sin(v * 3.14159 + 2.0), sin(v * 3.14159 + 4.0));
    frag_color = vec4(color * 0.5 + 0.5, 1.0);
}`
			return NewScreen(ctx, nil, fragmentSource), nil
		},
		stories.Config{
			Size: geom.Size{R: 30, C: 80},
		},
	)

	stories.Register(
		"opengl/camera-demo",
		func(ctx context.Context) (mux.Screen, error) {
			fragmentSource := `#version 330 core
out vec4 frag_color;

uniform float t;
uniform vec4 viewport;
uniform vec3 free_camera_target;
uniform vec2 free_camera_orbit;
uniform float free_camera_zoom;

void main() {
    vec2 uv = gl_FragCoord.xy / viewport.zw;
    
    // Use camera uniforms to create a moving pattern
    vec2 p = uv - 0.5;
    p *= free_camera_zoom;
    p += free_camera_orbit;
    
    float d = length(p);
    float a = atan(p.y, p.x);
    
    vec3 color = vec3(
        sin(d * 8.0 - t + free_camera_target.x),
        cos(a * 6.0 + t + free_camera_target.y),
        sin(d * a * 4.0 + t + free_camera_target.z)
    );
    
    frag_color = vec4(color * 0.5 + 0.5, 1.0);
}`
			return NewScreen(ctx, nil, fragmentSource), nil
		},
		stories.Config{
			Size: geom.Size{R: 25, C: 50},
		},
	)

	stories.Register(
		"opengl/shader-error",
		func(ctx context.Context) (mux.Screen, error) {
			// Invalid shader to test error display
			fragmentSource := `#version 330 core
out vec4 frag_color;

void main() {
    // This will cause a compilation error
    invalid_function_call();
    frag_color = vec4(1.0);
}`
			return NewScreen(ctx, nil, fragmentSource), nil
		},
		stories.Config{
			Size: geom.Size{R: 15, C: 60},
		},
	)
}
