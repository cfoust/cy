package overlay

import (
	"image"
	"image/color"
	"image/draw"
	"math"

	"golang.org/x/image/vector"
)

// The cursor SVG from cy-player.js:
//
//	viewBox="0 0 24 30", rendered at width=40 height=48
//	path: M2 2L2 24L7.5 18.5L12 28L16 26L11 16.5L19 16.5L2 2Z
//	fill + stroke="#1a1a1a" stroke-width="1.8"
const (
	cursorWidth  = 40
	cursorHeight = 48
	// viewBox 24x30 → 40x48
	cursorScaleX = float32(cursorWidth) / 24.0
	cursorScaleY = float32(cursorHeight) / 30.0
)

// SVG path vertices in viewBox coordinates.
var cursorPath = [][2]float32{
	{2, 2},
	{2, 24},
	{7.5, 18.5},
	{12, 28},
	{16, 26},
	{11, 16.5},
	{19, 16.5},
}

// rasterizeCursor draws the cursor arrow SVG path into a 40x48
// RGBA image with the given fill color and a dark outline.
func rasterizeCursor(fill color.RGBA) *image.RGBA {
	img := image.NewRGBA(
		image.Rect(0, 0, cursorWidth, cursorHeight),
	)

	// Rasterize the filled polygon as an alpha mask.
	r := vector.NewRasterizer(cursorWidth, cursorHeight)
	r.MoveTo(
		cursorPath[0][0]*cursorScaleX,
		cursorPath[0][1]*cursorScaleY,
	)
	for _, p := range cursorPath[1:] {
		r.LineTo(p[0]*cursorScaleX, p[1]*cursorScaleY)
	}
	r.ClosePath()

	mask := image.NewAlpha(img.Bounds())
	r.Draw(mask, mask.Bounds(), image.Opaque, image.Point{})

	// Draw a dark outline by dilating the mask.
	stroke := color.RGBA{R: 26, G: 26, B: 26, A: 255}
	outline := dilate(mask, 2)
	draw.DrawMask(
		img, img.Bounds(),
		image.NewUniform(stroke), image.Point{},
		outline, image.Point{},
		draw.Over,
	)

	// Draw the fill on top.
	draw.DrawMask(
		img, img.Bounds(),
		image.NewUniform(fill), image.Point{},
		mask, image.Point{},
		draw.Over,
	)

	return img
}

// dilate expands an alpha mask by radius pixels using a circular
// structuring element (Euclidean distance).
func dilate(src *image.Alpha, radius int) *image.Alpha {
	b := src.Bounds()
	dst := image.NewAlpha(b)
	r2 := float64(radius * radius)

	for y := b.Min.Y; y < b.Max.Y; y++ {
		for x := b.Min.X; x < b.Max.X; x++ {
			var best uint8
			for dy := -radius; dy <= radius; dy++ {
				for dx := -radius; dx <= radius; dx++ {
					if float64(dx*dx+dy*dy) > r2 {
						continue
					}
					sx, sy := x+dx, y+dy
					if sx < b.Min.X || sx >= b.Max.X ||
						sy < b.Min.Y || sy >= b.Max.Y {
						continue
					}
					if a := src.AlphaAt(sx, sy).A; a > best {
						best = a
					}
				}
			}
			dst.SetAlpha(x, y, color.Alpha{A: best})
		}
	}

	return dst
}

// cursorRenderer composites a cursor sprite onto an RGBA image at
// a position derived from terminal cell coordinates.
type cursorRenderer struct {
	white, red *image.RGBA
	colWidth   float64
	rowHeight  float64
	marginX    float64
	marginY    float64
	tipX       float64
	tipY       float64
}

func newCursorRenderer(
	fontSize, lineHeight float64,
) *cursorRenderer {
	colWidth := fontSize * 0.6
	rowHeight := fontSize * lineHeight

	return &cursorRenderer{
		white: rasterizeCursor(
			color.RGBA{R: 255, G: 255, B: 255, A: 255},
		),
		red: rasterizeCursor(
			color.RGBA{R: 255, G: 107, B: 107, A: 255},
		),
		colWidth:  colWidth,
		rowHeight: rowHeight,
		// agg's resvg layout: 1 column of horizontal
		// padding, half a row of vertical padding.
		marginX: colWidth,
		marginY: rowHeight / 2.0,
		// The cursor sprite tip at viewBox (2,2), scaled
		// from the SVG viewBox to the sprite's pixel size.
		tipX: 2.0 * float64(cursorScaleX),
		tipY: 2.0 * float64(cursorScaleY),
	}
}

// Rect returns the bounding rectangle of the cursor sprite when
// drawn at pos.
func (r *cursorRenderer) Rect(pos cursorPos) image.Rectangle {
	if !pos.Visible {
		return image.Rectangle{}
	}
	x := r.marginX + pos.Col*r.colWidth +
		r.colWidth/2.0 - r.tipX
	y := r.marginY + pos.Row*r.rowHeight +
		r.rowHeight/2.0 - r.tipY
	ix, iy := int(math.Round(x)), int(math.Round(y))
	return image.Rect(
		ix, iy,
		ix+cursorWidth, iy+cursorHeight,
	)
}

// Draw composites the cursor onto dst.
func (r *cursorRenderer) Draw(
	dst *image.RGBA,
	pos cursorPos,
) {
	if !pos.Visible {
		return
	}
	sprite := r.white
	if pos.Drag {
		sprite = r.red
	}
	rect := r.Rect(pos)
	draw.Draw(
		dst, rect,
		sprite, sprite.Bounds().Min,
		draw.Over,
	)
}

// PixelDist returns the pixel distance the cursor travels between
// two positions.
func (r *cursorRenderer) PixelDist(
	a, b cursorPos,
) float64 {
	dx := (b.Col - a.Col) * r.colWidth
	dy := (b.Row - a.Row) * r.rowHeight
	return math.Sqrt(dx*dx + dy*dy)
}

// ensureCursorColors adds white and red to the palette if not
// already present, so the cursor sprite quantizes reasonably.
func ensureCursorColors(
	palette color.Palette,
) color.Palette {
	needed := []color.Color{
		color.RGBA{R: 255, G: 255, B: 255, A: 255},
		color.RGBA{R: 255, G: 107, B: 107, A: 255},
		color.RGBA{R: 26, G: 26, B: 26, A: 255},
	}

	p := make(color.Palette, len(palette))
	copy(p, palette)

	for _, c := range needed {
		r1, g1, b1, _ := c.RGBA()
		found := false
		for _, existing := range p {
			r2, g2, b2, _ := existing.RGBA()
			if r1 == r2 && g1 == g2 && b1 == b2 {
				found = true
				break
			}
		}
		if !found && len(p) < 256 {
			p = append(p, c)
		}
	}

	return p
}
