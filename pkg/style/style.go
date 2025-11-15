package style

import (
	"fmt"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
)

var (
	KEYWORD_RIGHT  = janet.Keyword("right")
	KEYWORD_LEFT   = janet.Keyword("left")
	KEYWORD_CENTER = janet.Keyword("center")
	KEYWORD_TOP    = janet.Keyword("top")
	KEYWORD_BOTTOM = janet.Keyword("bottom")

	// Style property keywords
	KEYWORD_FG               = janet.Keyword("fg")
	KEYWORD_BG               = janet.Keyword("bg")
	KEYWORD_WIDTH            = janet.Keyword("width")
	KEYWORD_HEIGHT           = janet.Keyword("height")
	KEYWORD_ALIGN_HORIZONTAL = janet.Keyword("align-horizontal")
	KEYWORD_ALIGN_VERTICAL   = janet.Keyword("align-vertical")
	KEYWORD_BOLD             = janet.Keyword("bold")
	KEYWORD_ITALIC           = janet.Keyword("italic")
	KEYWORD_UNDERLINE        = janet.Keyword("underline")
	KEYWORD_STRIKETHROUGH    = janet.Keyword("strikethrough")
	KEYWORD_REVERSE          = janet.Keyword("reverse")
	KEYWORD_BLINK            = janet.Keyword("blink")
	KEYWORD_FAINT            = janet.Keyword("faint")
)

// We use a common renderer, since this actually has no impact on how/where we
// can render this style (it's all virtual anyway.)
var renderer *taro.Renderer = func() *taro.Renderer {
	return taro.NewRenderer()
}()

type Style struct {
	lipgloss.Style
}

var _ janet.Unmarshalable = (*Style)(nil)
var _ janet.Marshalable = (*Style)(nil)

type janetStyle struct {
	Fg              *janet.Value
	Bg              *janet.Value
	Width           *int
	Height          *int
	AlignHorizontal *janet.Value
	AlignVertical   *janet.Value
	Bold            *bool
	Italic          *bool
	Underline       *bool
	Strikethrough   *bool
	Reverse         *bool
	Blink           *bool
	Faint           *bool
}

func unmarshalPosition(value *janet.Value) (
	position lipgloss.Position,
	err error,
) {
	var keyword janet.Keyword
	err = value.Unmarshal(&keyword)
	if err != nil {
		return
	}

	switch keyword {
	case KEYWORD_RIGHT:
		return lipgloss.Right, nil
	case KEYWORD_LEFT:
		return lipgloss.Left, nil
	case KEYWORD_CENTER:
		return lipgloss.Center, nil
	case KEYWORD_TOP:
		return lipgloss.Top, nil
	case KEYWORD_BOTTOM:
		return lipgloss.Bottom, nil
	}

	err = fmt.Errorf("unknown position: %s", keyword)
	return
}

func (s *Style) UnmarshalJanet(value *janet.Value) (err error) {
	style := renderer.NewStyle()

	var v janetStyle
	if err := value.Unmarshal(&v); err != nil {
		return err
	}

	if !v.Fg.Nil() {
		var color Color
		err = v.Fg.Unmarshal(&color)
		if err != nil {
			return err
		}
		style = style.Foreground(color.Color)
	}

	if !v.Bg.Nil() {
		var color Color
		err = v.Bg.Unmarshal(&color)
		if err != nil {
			return err
		}
		style = style.Background(color.Color)
	}

	if v.Width != nil {
		style = style.Width(*v.Width)
	}

	if v.Height != nil {
		style = style.Height(*v.Height)
	}

	if v.Height != nil {
		style = style.Height(*v.Height)
	}

	if !v.AlignHorizontal.Nil() {
		var position lipgloss.Position
		position, err = unmarshalPosition(v.AlignHorizontal)
		if err != nil {
			return err
		}
		style = style.AlignHorizontal(position)
	}

	if !v.AlignVertical.Nil() {
		var position lipgloss.Position
		position, err = unmarshalPosition(v.AlignVertical)
		if err != nil {
			return err
		}
		style = style.AlignVertical(position)
	}

	if v.Bold != nil {
		style = style.Bold(*v.Bold)
	}

	if v.Italic != nil {
		style = style.Italic(*v.Italic)
	}

	if v.Underline != nil {
		style = style.Underline(*v.Underline)
	}

	if v.Strikethrough != nil {
		style = style.Strikethrough(*v.Strikethrough)
	}

	if v.Reverse != nil {
		style = style.Reverse(*v.Reverse)
	}

	if v.Blink != nil {
		style = style.Blink(*v.Blink)
	}

	if v.Faint != nil {
		style = style.Faint(*v.Faint)
	}

	s.Style = style
	return nil
}

// NewStyle creates a new Style with the given foreground and background colors
func NewStyle(fg, bg *Color) *Style {
	style := renderer.NewStyle()
	if fg != nil {
		style = style.Foreground(fg.Color)
	}
	if bg != nil {
		style = style.Background(bg.Color)
	}
	return &Style{Style: style}
}

// GetForegroundColor returns the foreground color as a *Color
func (s *Style) GetForegroundColor() *Color {
	if c, ok := s.GetForeground().(lipgloss.Color); ok {
		return &Color{Color: c}
	}
	return nil
}

// GetBackgroundColor returns the background color as a *Color
func (s *Style) GetBackgroundColor() *Color {
	if c, ok := s.GetBackground().(lipgloss.Color); ok {
		return &Color{Color: c}
	}
	return nil
}

// marshalHorizontalPosition converts a horizontal lipgloss.Position to a janet.Keyword
func marshalHorizontalPosition(position lipgloss.Position) janet.Keyword {
	switch position {
	case lipgloss.Right:
		return KEYWORD_RIGHT
	case lipgloss.Left:
		return KEYWORD_LEFT
	case lipgloss.Center:
		return KEYWORD_CENTER
	}
	return KEYWORD_LEFT // default fallback
}

// marshalVerticalPosition converts a vertical lipgloss.Position to a janet.Keyword
func marshalVerticalPosition(position lipgloss.Position) janet.Keyword {
	switch position {
	case lipgloss.Top:
		return KEYWORD_TOP
	case lipgloss.Bottom:
		return KEYWORD_BOTTOM
	case lipgloss.Center:
		return KEYWORD_CENTER
	}
	return KEYWORD_TOP // default fallback
}

// MarshalJanet implements the janet.Marshalable interface
func (s *Style) MarshalJanet() interface{} {
	if s == nil {
		return nil
	}

	result := make(map[janet.Keyword]interface{})

	// Extract foreground color if set
	if fg := s.GetForegroundColor(); fg != nil {
		result[KEYWORD_FG] = fg.MarshalJanet()
	}

	// Extract background color if set
	if bg := s.GetBackgroundColor(); bg != nil {
		result[KEYWORD_BG] = bg.MarshalJanet()
	}

	// Extract width if set
	if width := s.GetWidth(); width > 0 {
		result[KEYWORD_WIDTH] = width
	}

	// Extract height if set
	if height := s.GetHeight(); height > 0 {
		result[KEYWORD_HEIGHT] = height
	}

	// Extract horizontal alignment if set
	if hAlign := s.GetAlignHorizontal(); hAlign != lipgloss.Position(0) {
		result[KEYWORD_ALIGN_HORIZONTAL] = marshalHorizontalPosition(hAlign)
	}

	// Extract vertical alignment if set
	if vAlign := s.GetAlignVertical(); vAlign != lipgloss.Position(0) {
		result[KEYWORD_ALIGN_VERTICAL] = marshalVerticalPosition(vAlign)
	}

	// Extract text formatting flags
	if s.GetBold() {
		result[KEYWORD_BOLD] = true
	}
	if s.GetItalic() {
		result[KEYWORD_ITALIC] = true
	}
	if s.GetUnderline() {
		result[KEYWORD_UNDERLINE] = true
	}
	if s.GetStrikethrough() {
		result[KEYWORD_STRIKETHROUGH] = true
	}
	if s.GetReverse() {
		result[KEYWORD_REVERSE] = true
	}
	if s.GetBlink() {
		result[KEYWORD_BLINK] = true
	}
	if s.GetFaint() {
		result[KEYWORD_FAINT] = true
	}

	return result
}

func (s *Style) Apply(glyph *emu.Glyph) {
	if glyph == nil {
		return
	}

	if fg := s.GetForegroundColor(); fg != nil {
		glyph.FG = fg.Emu()
	}

	if bg := s.GetBackgroundColor(); bg != nil {
		glyph.BG = bg.Emu()
	}

	if s.GetBold() {
		glyph.Mode |= emu.AttrBold
	}
	if s.GetItalic() {
		glyph.Mode |= emu.AttrItalic
	}
	if s.GetUnderline() {
		glyph.Mode |= emu.AttrUnderline
	}
	if s.GetStrikethrough() {
		glyph.Mode |= emu.AttrStrikethrough
	}
	if s.GetReverse() {
		glyph.Mode |= emu.AttrReverse
	}
	if s.GetBlink() {
		glyph.Mode |= emu.AttrBlink
	}

	// TODO(cfoust): 08/16/25 ???
	//if s.GetFaint() {
	//}
}
