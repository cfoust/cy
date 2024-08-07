package style

import (
	"fmt"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/janet"

	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
)

var (
	KEYWORD_RIGHT  = janet.Keyword("right")
	KEYWORD_LEFT   = janet.Keyword("left")
	KEYWORD_CENTER = janet.Keyword("center")
	KEYWORD_TOP    = janet.Keyword("top")
	KEYWORD_BOTTOM = janet.Keyword("bottom")
)

// We use a common renderer, since this actually has no impact on how/where we
// can render this style (it's all virtual anyway.)
var renderer *lipgloss.Renderer = func() *lipgloss.Renderer {
	renderer := lipgloss.NewRenderer(emu.New())
	renderer.SetColorProfile(termenv.TrueColor)
	return renderer
}()

type Style struct {
	lipgloss.Style
}

var _ janet.Unmarshalable = (*Style)(nil)

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
