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
	border *Border,
) {
	char := image[row][col].Char
	if char != border.leftRune && char != border.topRune {
		return
	}

	var top, left, bottom, right bool
	switch char {
	case border.leftRune:
		top = true
		bottom = true
	case border.topRune:
		left = true
		right = true
	}

	if !left && col > 0 {
		left = image[row][col-1].Char == border.topRune
	}

	if !right && col < cols-1 {
		right = image[row][col+1].Char == border.topRune
	}

	if !top && row > 0 {
		top = image[row-1][col].Char == border.leftRune
	}

	if !bottom && row < rows-1 {
		bottom = image[row+1][col].Char == border.leftRune
	}

	if !left && !right && top && bottom {
		return
	}

	if left && right && !top && !bottom {
		return
	}

	if left && right && top && bottom {
		char = border.middleRune
	} else if !left && right && top && bottom {
		char = border.middleLeftRune
	} else if left && !right && top && bottom {
		char = border.middleRightRune
	} else if left && right && !top && !bottom {
		char = border.middleTopRune
	} else if left && right && top && !bottom {
		char = border.middleBottomRune
	}

	image[row][col].Char = char
}

type Border struct {
	// a "none" border is one that is explicitly hidden, which is
	// supported in some layout nodes
	isNone bool
	lipgloss.Border
	// Pre-computed rune values to avoid []rune() conversions in hot paths
	leftRune        rune
	topRune         rune
	middleRune      rune
	middleLeftRune  rune
	middleRightRune rune
	middleTopRune   rune
	middleBottomRune rune
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

func firstRune(s string) rune {
	for _, r := range s {
		return r
	}
	return 0
}

func NewBorder(border lipgloss.Border) Border {
	return Border{
		Border:           border,
		leftRune:         firstRune(border.Left),
		topRune:          firstRune(border.Top),
		middleRune:       firstRune(border.Middle),
		middleLeftRune:   firstRune(border.MiddleLeft),
		middleRightRune:  firstRune(border.MiddleRight),
		middleTopRune:    firstRune(border.MiddleTop),
		middleBottomRune: firstRune(border.MiddleBottom),
	}
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

	*b = NewBorder(border)
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
