package layout

import (
	"github.com/cfoust/cy/pkg/geom/image"

	"github.com/charmbracelet/lipgloss"
)

var borders = []lipgloss.Border{
	lipgloss.NormalBorder(),
	lipgloss.RoundedBorder(),
	lipgloss.BlockBorder(),
	lipgloss.OuterHalfBlockBorder(),
	lipgloss.InnerHalfBlockBorder(),
	lipgloss.ThickBorder(),
	lipgloss.DoubleBorder(),
	lipgloss.HiddenBorder(),
}

// fillBorder detects whether the cell specified by row and col can be replaced
// with a different border glyph that links its adjacent border cells together
// nicely.
func fillBorder(
	image image.Image,
	row, col, rows, cols int,
	border lipgloss.Border,
) {
	leftChar := []rune(border.Left)[0]
	topChar := []rune(border.Top)[0]
	char := image[row][col].Char
	if char != leftChar && char != topChar {
		return
	}

	var top, left, bottom, right bool
	if char == leftChar {
		top = true
		bottom = true
	} else if char == topChar {
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
	} else if left && right && top && !bottom {
		char = []rune(border.MiddleTop)[0]
	} else if left && right && !top && bottom {
		char = []rune(border.MiddleBottom)[0]
	}

	image[row][col].Char = char
}
