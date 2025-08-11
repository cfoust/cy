package opengl

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/stories"
)

// Simple color gradient story
var ColorGradient stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	fragmentSource := `#version 330 core
out vec4 FragColor;

uniform vec2 iResolution;

void main() {
    vec2 uv = gl_FragCoord.xy / iResolution.xy;
    vec3 color = vec3(uv.x, uv.y, 0.5);
    FragColor = vec4(color, 1.0);
}`

	return NewScreen(fragmentSource, 80, 24)
}

// Animated rainbow story with time uniform
var AnimatedRainbow stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	fragmentSource := `#version 330 core
out vec4 FragColor;

uniform float iTime;
uniform vec2 iResolution;

void main() {
    vec2 uv = gl_FragCoord.xy / iResolution.xy;
    vec3 color = 0.5 + 0.5 * cos(iTime + uv.xyx + vec3(0, 2, 4));
    FragColor = vec4(color, 1.0);
}`

	return NewScreen(fragmentSource, 80, 24)
}

// Plasma effect story
var PlasmaEffect stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	fragmentSource := `#version 330 core
out vec4 FragColor;

uniform float iTime;
uniform vec2 iResolution;

void main() {
    vec2 uv = (gl_FragCoord.xy / iResolution.xy) * 2.0 - 1.0;
    uv.x *= iResolution.x / iResolution.y;
    
    float time = iTime * 0.5;
    float plasma = sin(length(uv) * 10.0 - time * 2.0);
    plasma += sin(uv.x * 10.0 + time);
    plasma += sin(uv.y * 10.0 + time * 1.5);
    plasma += sin((uv.x + uv.y) * 10.0 + time * 0.5);
    plasma *= 0.25;
    
    vec3 color = vec3(
        0.5 + 0.5 * sin(plasma + 0.0),
        0.5 + 0.5 * sin(plasma + 2.0),
        0.5 + 0.5 * sin(plasma + 4.0)
    );
    
    FragColor = vec4(color, 1.0);
}`

	return NewScreen(fragmentSource, 80, 24)
}

// Mandelbrot fractal story  
var MandelbrotFractal stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	fragmentSource := `#version 330 core
out vec4 FragColor;

uniform float iTime;
uniform vec2 iResolution;

vec3 mandelbrot(vec2 c) {
    vec2 z = vec2(0.0);
    int iterations = 0;
    const int maxIter = 64;
    
    for(int i = 0; i < maxIter; i++) {
        if(length(z) > 2.0) break;
        z = vec2(z.x*z.x - z.y*z.y, 2.0*z.x*z.y) + c;
        iterations++;
    }
    
    float t = float(iterations) / float(maxIter);
    return vec3(
        0.5 + 0.5 * cos(3.0 + t * 3.0 + vec3(0.0, 0.6, 1.0))
    );
}

void main() {
    vec2 uv = (gl_FragCoord.xy / iResolution.xy - 0.5) * 4.0;
    uv.x *= iResolution.x / iResolution.y;
    
    // Slowly zoom into an interesting area
    float zoom = 0.5 + 0.3 * sin(iTime * 0.2);
    vec2 center = vec2(-0.7269, 0.1889);
    uv = uv * zoom + center;
    
    vec3 color = mandelbrot(uv);
    FragColor = vec4(color, 1.0);
}`

	return NewScreen(fragmentSource, 80, 30)
}

// Matrix rain effect story
var MatrixRain stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	fragmentSource := `#version 330 core
out vec4 FragColor;

uniform float iTime;
uniform vec2 iResolution;

float random(vec2 st) {
    return fract(sin(dot(st.xy, vec2(12.9898, 78.233))) * 43758.5453123);
}

void main() {
    vec2 uv = gl_FragCoord.xy / iResolution.xy;
    vec2 grid = floor(uv * vec2(20.0, 40.0));
    
    float time = iTime * 2.0;
    float rain = random(grid + floor(time)) * 
                random(grid + floor(time * 0.7));
    
    float trail = smoothstep(0.1, 0.9, 
        fract((uv.y * 40.0) + time * 5.0 * random(vec2(grid.x, 0.0))));
    
    vec3 color = vec3(0.0, rain * trail, rain * trail * 0.5);
    color *= 1.0 + 0.3 * sin(time + uv.y * 20.0);
    
    FragColor = vec4(color, 1.0);
}`

	return NewScreen(fragmentSource, 60, 30)
}

// Simple geometric pattern story
var GeometricPattern stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	fragmentSource := `#version 330 core
out vec4 FragColor;

uniform float iTime;
uniform vec2 iResolution;

void main() {
    vec2 uv = gl_FragCoord.xy / iResolution.xy;
    uv = uv * 2.0 - 1.0;
    uv.x *= iResolution.x / iResolution.y;
    
    float d = length(uv);
    float angle = atan(uv.y, uv.x);
    
    float pattern = sin(d * 10.0 - iTime * 2.0) * 
                   sin(angle * 8.0 + iTime);
    
    vec3 color = vec3(0.5 + 0.5 * pattern);
    color *= vec3(0.8, 0.9, 1.0); // Slight blue tint
    
    FragColor = vec4(color, 1.0);
}`

	return NewScreen(fragmentSource, 80, 24)
}

// Fire effect story
var FireEffect stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	fragmentSource := `#version 330 core
out vec4 FragColor;

uniform float iTime;
uniform vec2 iResolution;

float noise(vec2 p) {
    return fract(sin(dot(p, vec2(12.9898, 78.233))) * 43758.5453);
}

float fbm(vec2 p) {
    float value = 0.0;
    float amplitude = 0.5;
    for(int i = 0; i < 4; i++) {
        value += amplitude * noise(p);
        p *= 2.0;
        amplitude *= 0.5;
    }
    return value;
}

void main() {
    vec2 uv = gl_FragCoord.xy / iResolution.xy;
    uv.x *= iResolution.x / iResolution.y;
    
    // Create flame shape
    float flame = fbm(uv * 3.0 + vec2(0.0, -iTime * 2.0)) * 
                  (1.0 - uv.y) * (1.0 - uv.y);
    
    // Fire colors
    vec3 color = vec3(1.0, 0.5, 0.1) * flame +
                 vec3(1.0, 0.0, 0.0) * flame * flame +
                 vec3(1.0, 1.0, 0.0) * flame * flame * flame;
    
    FragColor = vec4(color, 1.0);
}`

	return NewScreen(fragmentSource, 80, 30)
}

func init() {
	// Standard animation config
	animConfig := stories.Config{
		Size: geom.Size{R: 24, C: 80},
		Input: []interface{}{
			stories.Wait(stories.More), // Let the animation run for a while
		},
	}

	// Longer animation for complex effects
	longAnimConfig := stories.Config{
		Size: geom.Size{R: 30, C: 80},
		Input: []interface{}{
			stories.Wait(stories.ALot), // Longer viewing time
		},
	}

	// Compact config for simple effects
	compactConfig := stories.Config{
		Size: geom.Size{R: 20, C: 60},
		Input: []interface{}{
			stories.Wait(stories.Some),
		},
	}

	// Register all stories
	stories.Register("rasterion/opengl/gradient", ColorGradient, compactConfig)
	stories.Register("rasterion/opengl/rainbow", AnimatedRainbow, animConfig)
	stories.Register("rasterion/opengl/plasma", PlasmaEffect, animConfig)
	stories.Register("rasterion/opengl/mandelbrot", MandelbrotFractal, longAnimConfig)
	stories.Register("rasterion/opengl/matrix", MatrixRain, longAnimConfig)
	stories.Register("rasterion/opengl/geometric", GeometricPattern, animConfig)
	stories.Register("rasterion/opengl/fire", FireEffect, longAnimConfig)
}
