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

	stories.Register(
		"opengl/balloon",
		func(ctx context.Context) (mux.Screen, error) {
			fragmentSource := `#version 330
precision highp float;

struct Ray {
  vec3 origin;
  vec3 direction;
};
struct Light {
  vec3 color;
  vec3 direction;
  float brightness;
};

out vec4 frag_color;

uniform float free_camera_zoom;
uniform vec3 free_camera_target;
uniform vec2 free_camera_orbit;
uniform float t;
uniform vec4 viewport;

mat2 rotation_2d(float angle) {
  float s = sin(angle);
  float c = cos(angle);
  return mat2(c, s, -s, c);
}

float max_(vec2 v) {
  return max(v.x, v.y);
}

mat3 rotation_y(float angle) {
  float s = sin(angle);
  float c = cos(angle);
  return mat3(c, 0.0, -s, 0.0, 1.0, 0.0, s, 0.0, c);
}

mat3 rotation_x(float angle) {
  float s = sin(angle);
  float c = cos(angle);
  return mat3(1.0, 0.0, 0.0, 0.0, c, s, 0.0, -s, c);
}

vec3 perspective_vector(float fov, vec2 frag_coord) {
  float cot_half_fov = tan(radians(90.0 - (fov * 0.5)));
  return normalize(vec3(frag_coord, cot_half_fov));
}

float atan2(float y, float x) {
  return (x == 0.0) ? ((0.5 * 3.14159265358979) * sign(y)) : atan(y, x);
}

float atan21(vec2 p) {
  return atan2(p.y, p.x);
}

float sdf_ellipsoid(vec3 size, vec3 p) {
  float k0 = length(p / size);
  float k1 = length(p / (size * size));
  return (k0 * (k0 - 1.0)) / k1;
}

float sdf_cylinder_y(float radius, float height, vec3 p) {
  vec2 other_axes = p.xz;
  float this_axis = p.y;
  vec2 d = abs(vec2(length(other_axes), this_axis)) - vec2(radius, height);
  return min(max_(d), 0.0) + length(max(d, 0.0));
}

float move_outer(vec3 p) {
  {
    vec3 p1 = p - (vec3(0.0, -100.0, 0.0) * 1.0);
    return sdf_cylinder_y(25.0, 50.0, p1);
  }
}

float smooth_min_distance(vec3 p) {
  float r = 50.0;
  float nearest = sdf_ellipsoid(vec3(50.0, 100.0, 100.0), p);
  float dist = move_outer(p);
  float h = (clamp((nearest - dist) / r, -1.0, 1.0) + 1.0) * 0.5;
  nearest = mix(nearest, dist, h) - (r * h * (1.0 - h));
  return nearest;
}

float scale_outer(vec3 factor, vec3 p) {
  {
    vec3 p1 = p / factor;
    return smooth_min_distance(p1);
  }
}

float min_(vec3 v) {
  return min(v.x, min(v.y, v.z));
}

float map_distance(vec3 factor, vec3 p) {
  float dist = scale_outer(factor, p);
  return min_(abs(factor)) * dist;
}

float let_outer(vec3 p) {
  {
    vec3 factor = (1.0 - vec3(0.0, 1.0, 0.0)) + (vec3(0.0, 1.0, 0.0) * ((smoothstep(-100.0, 100.0, p.y) * -0.2) + 1.0));
    return map_distance(factor, p);
  }
}

float with_outer(float angular_size, vec3 p) {
  {
    float radial_index = floor(mod(atan21(p.zx) + (angular_size * 0.5), 6.28318530717959) / angular_size);
    vec3 p1 = rotation_y(-1.0 * angular_size * radial_index) * p;
    return let_outer(p1);
  }
}

float let_outer1(vec3 p) {
  {
    float count = 18.0;
    float angular_size = 6.28318530717959 / count;
    return with_outer(angular_size, p);
  }
}

float max_distance(vec3 p, float t) {
  float nearest = abs(let_outer1(p)) - (1.0 * 0.5);
  nearest = max(nearest, -(dot(p, vec3(-1.0, 0.0, 0.0)) - ((150.0 * (1.0 - ((cos((6.28318530717959 * t) / 10.0) + 1.0) * 0.5))) + -150.0)));
  return nearest;
}

float nearest_distance(vec3 p, float t) {
  return max_distance(p, t);
}

float march(out uint steps, Ray ray, float t) {
  float ray_depth = 0.0;
  for (steps = 0u; steps < 256u; ++steps) {
    {
      float depth = ray_depth;
      vec3 P = ray.origin + (ray_depth * ray.direction);
      vec3 p = P;
      float dist = nearest_distance(p, t);
      if (((dist >= 0.0) && (dist < 0.1)) || (ray_depth > 65536.0)) return ray_depth;
      float rate = (dist > 0.0) ? 0.95 : 1.05;
      ray_depth += dist * rate;
      if (ray_depth < 0.0) return 0.0;
    }
  }
  return ray_depth;
}

float with_outer1(vec3 p, float t) {
  {
    vec3 p1 = (vec2(1.0, -1.0).xyy * 0.005) + p;
    return nearest_distance(p1, t);
  }
}

float with_outer2(vec3 p, float t) {
  {
    vec3 p1 = (vec2(1.0, -1.0).yyx * 0.005) + p;
    return nearest_distance(p1, t);
  }
}

float with_outer3(vec3 p, float t) {
  {
    vec3 p1 = (vec2(1.0, -1.0).yxy * 0.005) + p;
    return nearest_distance(p1, t);
  }
}

float with_outer4(vec3 p, float t) {
  {
    vec3 p1 = (vec2(1.0, -1.0).xxx * 0.005) + p;
    return nearest_distance(p1, t);
  }
}

vec3 do_(vec2 Frag_Coord, vec2 resolution) {
  const vec3 light = pow(vec3(69.0, 72.0, 79.0) / 255.0, vec3(2.2));
  const vec3 dark = pow(vec3(40.0, 42.0, 46.0) / 255.0, vec3(2.2));
  return vec3(mix(dark, light, (Frag_Coord.x + Frag_Coord.y) / (resolution.x + resolution.y)));
}

float with_outer5(float depth, vec3 light_position, vec3 ray_dir, float t) {
  {
    vec3 P = light_position + (ray_dir * depth);
    vec3 p = P;
    return nearest_distance(p, t);
  }
}

Light cast_light_hard_shadow(vec3 light_color, vec3 light_position, vec3 P, vec3 normal, float t) {
  if (light_position == P) return Light(light_color, vec3(0.0), 1.0);
  vec3 to_light = normalize(light_position - P);
  if (light_color == vec3(0.0)) return Light(light_color, to_light, 0.0);
  if (dot(to_light, normal) < 0.0) return Light(light_color, to_light, 0.0);
  vec3 target = (0.01 * normal) + P;
  float light_distance = length(target - light_position);
  vec3 ray_dir = (target - light_position) / light_distance;
  float depth = 0.0;
  for (uint i = 0u; i < 256u; ++i) {
    float nearest = with_outer5(depth, light_position, ray_dir, t);
    if (nearest < 0.01) break;
    depth += nearest;
  }
  if (depth >= light_distance) return Light(light_color, to_light, 1.0);
  else return Light(light_color, to_light, 0.0);
}

float with_outer6(float depth, vec3 light_position, vec3 ray_dir, float t) {
  {
    vec3 P = light_position + (ray_dir * depth);
    vec3 p = P;
    return nearest_distance(p, t);
  }
}

Light cast_light_soft_shadow(vec3 light_color, vec3 light_position, float softness, vec3 P, vec3 normal, float t) {
  if (softness == 0.0) return cast_light_hard_shadow(light_color, light_position, P, normal, t);
  if (light_position == P) return Light(light_color, vec3(0.0), 1.0);
  vec3 to_light = normalize(light_position - P);
  if (light_color == vec3(0.0)) return Light(light_color, to_light, 0.0);
  if (dot(to_light, normal) < 0.0) return Light(light_color, to_light, 0.0);
  vec3 target = (0.01 * normal) + P;
  float light_distance = length(target - light_position);
  vec3 ray_dir = (target - light_position) / light_distance;
  float brightness = 1.0;
  float sharpness = 1.0 / (softness * softness);
  float last_nearest = 1000000.0;
  float depth = 0.0;
  for (uint i = 0u; i < 256u; ++i) {
    float nearest = with_outer6(depth, light_position, ray_dir, t);
    if (nearest < 0.01) break;
    float intersect_offset = (nearest * nearest) / (2.0 * last_nearest);
    float intersect_distance = sqrt((nearest * nearest) - (intersect_offset * intersect_offset));
    brightness = min(brightness, (sharpness * intersect_distance) / max(0.0, (light_distance - depth) - intersect_offset));
    depth += nearest;
    last_nearest = nearest;
  }
  if (depth >= light_distance) return Light(light_color, to_light, brightness);
  else return Light(light_color, to_light, 0.0);
}

float with_outer7(vec3 P, uint i, vec3 step, float t) {
  {
    vec3 P1 = (float(i) * step) + P;
    vec3 p = P1;
    return max(nearest_distance(p, t), 0.0);
  }
}

float calculate_occlusion(uint step_count, float max_distance, vec3 dir, vec3 P, vec3 p, float t) {
  float step_size = max_distance / float(step_count);
  float baseline = nearest_distance(p, t);
  float occlusion = 0.0;
  vec3 step = dir * step_size;
  for (uint i = 1u; i <= step_count; ++i) {
    float expected_distance = (float(i) * step_size) + baseline;
    float actual_distance = with_outer7(P, i, step, t);
    occlusion += actual_distance / expected_distance;
  }
  return clamp(occlusion / float(step_count), 0.0, 1.0);
}

vec3 normalize_safe(vec3 v) {
  return (v == vec3(0.0)) ? v : normalize(v);
}

Light cast_light_no_shadow(vec3 light_color, vec3 light_position, vec3 P) {
  return Light(light_color, normalize_safe(light_position - P), 1.0);
}

Light do_1(vec3 P, vec3 normal, float occlusion) {
  Light light = cast_light_no_shadow(vec3(0.15), P + (normal * 0.1), P);
  light.brightness = light.brightness * mix(0.1, 1.0, occlusion);
  return light;
}

uint union_color_index(vec3 p, float t) {
  float nearest = dot(p, vec3(-1.0, 0.0, 0.0)) - ((150.0 * (1.0 - ((cos((6.28318530717959 * t) / 10.0) + 1.0) * 0.5))) + -150.0);
  uint nearest_index = 0u;
  float d = dot(p, vec3(-1.0, 0.0, 0.0)) - ((150.0 * (1.0 - ((cos((6.28318530717959 * t) / 10.0) + 1.0) * 0.5))) + -150.0);
  if (d < 0.0) return 0u;
  if (d < nearest) {
    nearest = d;
    nearest_index = 0u;
  }
  float d1 = abs(let_outer1(p)) - (1.0 * 0.5);
  if (d1 < 0.0) return 1u;
  if (d1 < nearest) {
    nearest = d1;
    nearest_index = 1u;
  }
  return nearest_index;
}

mat3 rotation_z(float angle) {
  float s = sin(angle);
  float c = cos(angle);
  return mat3(c, s, 0.0, -s, c, 0.0, 0.0, 0.0, 1.0);
}

vec3 hsv(float hue, float saturation, float value) {
  vec3 c = abs(mod((hue * 6.0) + vec3(0.0, 4.0, 2.0), 6.0) - 3.0);
  return value * mix(vec3(1.0), clamp(c - 1.0, 0.0, 1.0), saturation);
}

float let_outer2(vec3 p, float t) {
  {
    float dollar_count = 8.0;
    return round(((p.y / 100.0) - (t / 5.0)) * dollar_count) / dollar_count;
  }
}

vec3 blinn_phong(Light light, vec3 color, float shininess, float glossiness, vec3 normal, Ray ray) {
  if (light.direction == vec3(0.0)) return color * light.color * light.brightness;
  vec3 halfway_dir = normalize(light.direction - ray.direction);
  float specular_strength = shininess * pow(max(dot(normal, halfway_dir), 0.0), glossiness * glossiness);
  float diffuse = max(0.0, dot(normal, light.direction));
  return ((light.color * light.brightness) * specular_strength) + (color * diffuse * light.color * light.brightness);
}

vec3 shade(Light light, Light light1, vec3 normal, Ray ray, vec3 temp) {
  vec3 result = vec3(0.0);
  result += blinn_phong(light1, temp, 0.25, 5.0, normal, ray);
  result += blinn_phong(light, temp, 0.25, 5.0, normal, ray);
  return result;
}

vec3 shade_outer(Light light, Light light1, vec3 normal, vec3 p, Ray ray, float t) {
  {
    vec3 temp = hsv(let_outer2(p, t), 1.0, 1.0);
    return shade(light1, light, normal, ray, temp);
  }
}

vec3 move_outer1(Light light, Light light1, vec3 normal, vec3 p, float parity, Ray ray, float t) {
  {
    vec3 p1 = p - (vec3(0.0, 1.0, 0.0) * (((smoothstep(0.0, 1.0, sin(t * 1.5)) * 100.0) / 8.0) * parity));
    return shade_outer(light, light1, normal, p1, ray, t);
  }
}

vec3 rotate_outer(Light light, Light light1, vec3 normal, vec3 p, float parity, Ray ray, float t) {
  {
    vec3 p1 = p * rotation_z(((parity * 2.0) - 1.0) * 0.785398163397448);
    return move_outer1(light, light1, normal, p1, parity, ray, t);
  }
}

vec3 let_outer3(Light light, Light light1, vec3 normal, vec3 p, float radial_index, Ray ray, float t) {
  {
    float parity = mod(radial_index, 2.0);
    return rotate_outer(light1, light, normal, p, parity, ray, t);
  }
}

vec3 with_outer8(float angular_size, Light light, Light light1, vec3 normal, vec3 p, Ray ray, float t) {
  {
    float radial_index = floor(mod(atan21(p.zx) + (angular_size * 0.5), 6.28318530717959) / angular_size);
    vec3 p1 = rotation_y(-1.0 * angular_size * radial_index) * p;
    return let_outer3(light1, light, normal, p1, radial_index, ray, t);
  }
}

vec3 let_outer4(Light light, Light light1, vec3 normal, vec3 p, Ray ray, float t) {
  {
    float count = 18.0;
    float angular_size = 6.28318530717959 / count;
    return with_outer8(angular_size, light, light1, normal, p, ray, t);
  }
}

vec3 union_color(Light light, Light light1, vec3 normal, vec3 p, Ray ray, float t) {
  switch (union_color_index(p, t)) {
  case 0u: return vec3(0.0, 1.0, 1.0);
  case 1u: return let_outer4(light, light1, normal, p, ray, t);
  }
  return vec3(-1.0, 0.0, 0.0);
}

float fresnel(float exponent, vec3 normal, Ray ray) {
  return pow(1.0 + dot(normal, ray.direction), exponent);
}

vec3 map_color(Light light, Light light1, vec3 normal, vec3 p, Ray ray, float t) {
  vec3 color = union_color(light, light1, normal, p, ray, t);
  return color + (vec3(0.5, 0.5, 1.0) * (fresnel(5.0, normal, ray) * 0.2));
}

vec3 hoist_outer(vec3 P, vec3 normal, vec3 p, Ray ray, float t) {
  {
    Light light = cast_light_soft_shadow(vec3(1.15), P - (normalize(vec3(-2.0, -2.0, -1.0)) * 2048.0), 0.25, P, normal, t);
    float occlusion = calculate_occlusion(8u, 20.0, normal, P, p, t);
    Light light1 = do_1(P, normal, occlusion);
    return map_color(light, light1, normal, p, ray, t);
  }
}

vec4 sample_(vec2 Frag_Coord, vec2 frag_coord, vec2 free_camera_orbit, float free_camera_zoom, vec3 free_camera_target, vec2 resolution, float t) {
  Ray ray_star = Ray(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 1.0));
  vec3 ortho_quad = vec3(0.0, 0.0, 0.0);
  float ortho_scale = 0.0;
  float fov = 0.0;
  mat3 camera_rotation_matrix = rotation_y(6.28318530717959 * free_camera_orbit.x) * rotation_x(6.28318530717959 * free_camera_orbit.y);
  ray_star = Ray((camera_rotation_matrix * vec3(0.0, 0.0, 512.0 * free_camera_zoom)) + free_camera_target, camera_rotation_matrix * (perspective_vector(45.0, frag_coord) * vec3(1.0, 1.0, -1.0)));
  uint steps = 0u;
  {
    Ray ray = ray_star;
    float depth = march(steps, ray, t);
    vec3 P = ray.origin + (ray.direction * depth);
    vec3 p = P;
    float dist = nearest_distance(p, t);
    vec3 normal = normalize((vec2(1.0, -1.0).xyy * with_outer1(p, t)) + (vec2(1.0, -1.0).yyx * with_outer2(p, t)) + (vec2(1.0, -1.0).yxy * with_outer3(p, t)) + (vec2(1.0, -1.0).xxx * with_outer4(p, t)));
    vec4 color = vec4(0.0);
    color = (dist >= 10.0) ? vec4(do_(Frag_Coord, resolution), 1.0) : vec4(hoist_outer(P, normal, p, ray, t), 1.0);
    return color;
  }
}

vec3 pow_(vec3 v, float e) {
  return pow(v, vec3(e));
}

void main() {
  const float gamma = 2.2;
  vec3 color = vec3(0.0, 0.0, 0.0);
  float alpha = 0.0;
  const uint aa_grid_size = 1u;
  const float aa_sample_width = 1.0 / float(1u + aa_grid_size);
  const vec2 pixel_origin = vec2(0.5, 0.5);
  vec2 local_frag_coord = gl_FragCoord.xy - viewport.xy;
  mat2 rotation = rotation_2d(0.2);
  for (uint y = 1u; y <= aa_grid_size; ++y) {
    for (uint x = 1u; x <= aa_grid_size; ++x) {
      vec2 sample_offset = (aa_sample_width * vec2(float(x), float(y))) - pixel_origin;
      sample_offset = rotation * sample_offset;
      sample_offset = fract(sample_offset + pixel_origin) - pixel_origin;
      {
        vec2 Frag_Coord = local_frag_coord + sample_offset;
        vec2 resolution = viewport.zw;
        vec2 frag_coord = ((Frag_Coord - (0.5 * resolution)) / max_(resolution)) * 2.0;
        vec4 this_sample = clamp(sample_(Frag_Coord, frag_coord, free_camera_orbit, free_camera_zoom, free_camera_target, resolution, t), 0.0, 1.0);
        color += this_sample.rgb * this_sample.a;
        alpha += this_sample.a;
      }
    }
  }
  if (alpha > 0.0) {
    color = color / alpha;
    alpha /= float(aa_grid_size * aa_grid_size);
  }
  frag_color = vec4(pow_(color, 1.0 / gamma), alpha);
}`

			return NewScreen(ctx, nil, fragmentSource), nil
		},
		stories.Config{},
	)

	stories.Register(
		"opengl/glyphs",
		func(ctx context.Context) (mux.Screen, error) {
			fragmentSource := `
#version 330
precision highp float;

#define CHAR_97 1.0 // character '97'

struct Ray {
  vec3 origin;
  vec3 direction;
};

layout(location=0) out vec4 frag_color;
layout(location=1) out vec3 glyph;

uniform float free_camera_zoom;
uniform vec3 free_camera_target;
uniform vec2 free_camera_orbit;
uniform float t;
uniform vec4 viewport;

mat2 rotation_2d(float angle) {
  float s = sin(angle);
  float c = cos(angle);
  return mat2(c, s, -s, c);
}

float max_(vec2 v) {
  return max(v.x, v.y);
}

mat3 rotation_y(float angle) {
  float s = sin(angle);
  float c = cos(angle);
  return mat3(c, 0.0, -s, 0.0, 1.0, 0.0, s, 0.0, c);
}

mat3 rotation_x(float angle) {
  float s = sin(angle);
  float c = cos(angle);
  return mat3(1.0, 0.0, 0.0, 0.0, c, s, 0.0, -s, c);
}

vec3 perspective_vector(float fov, vec2 frag_coord) {
  float cot_half_fov = tan(radians(90.0 - (fov * 0.5)));
  return normalize(vec3(frag_coord, cot_half_fov));
}

float sdf_sphere(float radius, vec3 p) {
  return length(p) - radius;
}

float nearest_distance(vec3 p) {
  return sdf_sphere(100.0, p);
}

float march(out uint steps, Ray ray) {
  float ray_depth = 0.0;
  for (steps = 0u; steps < 256u; ++steps) {
    {
      float depth = ray_depth;
      vec3 P = ray.origin + (ray_depth * ray.direction);
      vec3 p = P;
      float dist = nearest_distance(p);
      if (((dist >= 0.0) && (dist < 0.1)) || (ray_depth > 65536.0)) return ray_depth;
      float rate = (dist > 0.0) ? 0.95 : 1.05;
      ray_depth += dist * rate;
      if (ray_depth < 0.0) return 0.0;
    }
  }
  return ray_depth;
}

float with_outer(vec3 p) {
  {
    vec3 p1 = (vec2(1.0, -1.0).xyy * 0.005) + p;
    return nearest_distance(p1);
  }
}

float with_outer1(vec3 p) {
  {
    vec3 p1 = (vec2(1.0, -1.0).yyx * 0.005) + p;
    return nearest_distance(p1);
  }
}

float with_outer2(vec3 p) {
  {
    vec3 p1 = (vec2(1.0, -1.0).yxy * 0.005) + p;
    return nearest_distance(p1);
  }
}

float with_outer3(vec3 p) {
  {
    vec3 p1 = (vec2(1.0, -1.0).xxx * 0.005) + p;
    return nearest_distance(p1);
  }
}

vec3 do_(vec2 Frag_Coord, vec2 resolution) {
  const vec3 light = pow(vec3(69.0, 72.0, 79.0) / 255.0, vec3(2.2));
  const vec3 dark = pow(vec3(40.0, 42.0, 46.0) / 255.0, vec3(2.2));
  return vec3(mix(dark, light, (Frag_Coord.x + Frag_Coord.y) / (resolution.x + resolution.y)));
}

vec4 sample_(out vec3 glyph, vec2 Frag_Coord, vec2 frag_coord, vec2 free_camera_orbit, float free_camera_zoom, vec3 free_camera_target, vec2 resolution) {
  Ray ray_star = Ray(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 1.0));
  vec3 ortho_quad = vec3(0.0, 0.0, 0.0);
  float ortho_scale = 0.0;
  float fov = 0.0;
  mat3 camera_rotation_matrix = rotation_y(6.28318530717959 * free_camera_orbit.x) * rotation_x(6.28318530717959 * free_camera_orbit.y);
  ray_star = Ray((camera_rotation_matrix * vec3(0.0, 0.0, 512.0 * free_camera_zoom)) + free_camera_target, camera_rotation_matrix * (perspective_vector(45.0, frag_coord) * vec3(1.0, 1.0, -1.0)));
  uint steps = 0u;
  {
    Ray ray = ray_star;
    float depth = march(steps, ray);
    vec3 P = ray.origin + (ray.direction * depth);
    vec3 p = P;
    float dist = nearest_distance(p);
    vec3 normal = normalize((vec2(1.0, -1.0).xyy * with_outer(p)) + (vec2(1.0, -1.0).yyx * with_outer1(p)) + (vec2(1.0, -1.0).yxy * with_outer2(p)) + (vec2(1.0, -1.0).xxx * with_outer3(p)));
    vec4 color = vec4(0.0);
    color = (dist >= 10.0) ? vec4(do_(Frag_Coord, resolution), 1.0) : vec4(vec3(0.0, 1.0, 1.0), 1.0);
    glyph = (dist >= 10.0) ? vec3(0.0, 0.0, 0.0) : vec3(vec3(float(1.0), 0.0, 0.0));
    return color;
  }
}

vec3 pow_(vec3 v, float e) {
  return pow(v, vec3(e));
}

void main() {
  const float gamma = 2.2;
  vec3 color = vec3(0.0, 0.0, 0.0);
  float alpha = 0.0;
  const uint aa_grid_size = 1u;
  const float aa_sample_width = 1.0 / float(1u + aa_grid_size);
  const vec2 pixel_origin = vec2(0.5, 0.5);
  vec2 local_frag_coord = gl_FragCoord.xy - viewport.xy;
  mat2 rotation = rotation_2d(0.2);
  for (uint y = 1u; y <= aa_grid_size; ++y) {
    for (uint x = 1u; x <= aa_grid_size; ++x) {
      vec2 sample_offset = (aa_sample_width * vec2(float(x), float(y))) - pixel_origin;
      sample_offset = rotation * sample_offset;
      sample_offset = fract(sample_offset + pixel_origin) - pixel_origin;
      {
        vec2 Frag_Coord = local_frag_coord + sample_offset;
        vec2 resolution = viewport.zw;
        vec2 frag_coord = ((Frag_Coord - (0.5 * resolution)) / max_(resolution)) * 2.0;
        vec4 this_sample = clamp(sample_(glyph, Frag_Coord, frag_coord, free_camera_orbit, free_camera_zoom, free_camera_target, resolution), 0.0, 1.0);
        color += this_sample.rgb * this_sample.a;
        alpha += this_sample.a;
      }
    }
  }
  if (alpha > 0.0) {
    color = color / alpha;
    alpha /= float(aa_grid_size * aa_grid_size);
  }
  frag_color = vec4(pow_(color, 1.0 / gamma), alpha);
} `

			return NewScreen(ctx, nil, fragmentSource), nil
		},
		stories.Config{},
	)
}
