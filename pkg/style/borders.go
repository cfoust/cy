package style

import (
	"fmt"

	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/janet"

	"github.com/charmbracelet/lipgloss"
)

var (
	KEYWORD_NORMAL     = janet.Keyword("normal")
	KEYWORD_ROUNDED    = janet.Keyword("rounded")
	KEYWORD_BLOCK      = janet.Keyword("block")
	KEYWORD_OUTER_HALF = janet.Keyword("outer-half")
	KEYWORD_INNER_HALF = janet.Keyword("inner-half")
	KEYWORD_THICK      = janet.Keyword("thick")
	KEYWORD_DOUBLE     = janet.Keyword("double")
	KEYWORD_HIDDEN     = janet.Keyword("hidden")

	// Special border behavior
	KEYWORD_NONE = janet.Keyword("none")
)

var Borders = []Border{
	NewBorder(lipgloss.NormalBorder()),
	NewBorder(lipgloss.RoundedBorder()),
	NewBorder(lipgloss.BlockBorder()),
	NewBorder(lipgloss.OuterHalfBlockBorder()),
	NewBorder(lipgloss.InnerHalfBlockBorder()),
	NewBorder(lipgloss.ThickBorder()),
	NewBorder(lipgloss.DoubleBorder()),
	NewBorder(lipgloss.HiddenBorder()),
}

var borderMap = map[janet.Keyword]lipgloss.Border{
	KEYWORD_NORMAL:     lipgloss.NormalBorder(),
	KEYWORD_ROUNDED:    lipgloss.RoundedBorder(),
	KEYWORD_BLOCK:      lipgloss.BlockBorder(),
	KEYWORD_OUTER_HALF: lipgloss.OuterHalfBlockBorder(),
	KEYWORD_INNER_HALF: lipgloss.InnerHalfBlockBorder(),
	KEYWORD_THICK:      lipgloss.ThickBorder(),
	KEYWORD_DOUBLE:     lipgloss.DoubleBorder(),
	KEYWORD_HIDDEN:     lipgloss.HiddenBorder(),
}

// FillBorder detects whether the cell specified by row and col can be replaced
// with a different border glyph that links its adjacent border cells together
// nicely.
func FillBorder(
	image image.Image,
	row, col, rows, cols int,
	border Border,
) {
	leftChar := []rune(border.Left)[0]
	topChar := []rune(border.Top)[0]
	char := image[row][col].Char
	if char != leftChar && char != topChar {
		return
	}

	var top, left, bottom, right bool
	switch char {
	case leftChar:
		top = true
		bottom = true
	case topChar:
		left = true
		right = true
	}

	if !left && col > 0 {
		left = image[row][col-1].Char == []rune(border.Top)[0]
	}

	if !right && col < cols-1 {
		right = image[row][col+1].Char == []rune(border.Top)[0]
	}

	if !top && row > 0 {
		top = image[row-1][col].Char == []rune(border.Left)[0]
	}

	if !bottom && row < rows-1 {
		bottom = image[row+1][col].Char == []rune(border.Left)[0]
	}

	if !left && !right && top && bottom {
		return
	}

	if left && right && !top && !bottom {
		return
	}

	if left && right && top && bottom {
		char = []rune(border.Middle)[0]
	} else if !left && right && top && bottom {
		char = []rune(border.MiddleLeft)[0]
	} else if left && !right && top && bottom {
		char = []rune(border.MiddleRight)[0]
	} else if left && right && !top && !bottom {
		char = []rune(border.MiddleTop)[0]
	} else if left && right && top && !bottom {
		char = []rune(border.MiddleBottom)[0]
	}

	image[row][col].Char = char
}

type Border struct {
	// a "none" border is one that is explicitly hidden, which is
	// supported in some layout nodes
	isNone bool
	lipgloss.Border
}

// None reports whether this Border is none, or should not be rendered.
func (b Border) None() bool {
	return b.isNone
}

func (b Border) Smoothable() bool {
	for _, s := range []string{
		b.Middle,
		b.MiddleLeft,
		b.MiddleRight,
		b.MiddleTop,
		b.MiddleBottom,
	} {
		if len(s) == 0 {
			return false
		}
	}

	return true
}

func NewBorder(border lipgloss.Border) Border {
	return Border{Border: border}
}

var DefaultBorder = NewBorder(lipgloss.RoundedBorder())

var _ janet.Unmarshalable = (*Border)(nil)

func (b *Border) UnmarshalJanet(value *janet.Value) (err error) {
	var keyword janet.Keyword
	err = value.Unmarshal(&keyword)
	if err != nil {
		return err
	}

	var border lipgloss.Border
	switch keyword {
	case KEYWORD_NORMAL:
		border = lipgloss.NormalBorder()
	case KEYWORD_ROUNDED:
		border = lipgloss.RoundedBorder()
	case KEYWORD_BLOCK:
		border = lipgloss.BlockBorder()
	case KEYWORD_OUTER_HALF:
		border = lipgloss.OuterHalfBlockBorder()
	case KEYWORD_INNER_HALF:
		border = lipgloss.InnerHalfBlockBorder()
	case KEYWORD_THICK:
		border = lipgloss.ThickBorder()
	case KEYWORD_DOUBLE:
		border = lipgloss.DoubleBorder()
	case KEYWORD_HIDDEN:
		border = lipgloss.HiddenBorder()
	case KEYWORD_NONE:
		b.isNone = true
		return nil
	default:
		return fmt.Errorf(
			"invalid border style: %s", keyword,
		)
	}

	b.Border = border
	return nil
}

var _ janet.Marshalable = (*Border)(nil)

func (b *Border) MarshalJanet() interface{} {
	if b.isNone {
		return KEYWORD_NONE
	}

	for key, value := range borderMap {
		if b.Border != value {
			continue
		}

		return key
	}

	return nil
}
