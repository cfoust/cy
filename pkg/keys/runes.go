package keys

import (
	"slices"
	"strconv"
)

func isPrint(r rune) bool {
	// Make sure rune is below special Kitty constants
	return strconv.IsPrint(r) && r < 0xE000
}

func onlyPrintable(runes []rune) []rune {
	return slices.Collect(func(yield func(r rune) bool) {
		for _, r := range runes {
			if !isPrint(r) {
				continue
			}

			yield(r)
		}
	})
}
