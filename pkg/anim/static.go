package anim

import (
	"math/rand"
	"time"
	"unicode/utf8"

	"github.com/cfoust/cy/pkg/geom/image"
)

type Static struct {
	in image.Image
}

var _ Animation = (*Static)(nil)

var BRAILLE = func() (runes []rune) {
	bytes := []byte{226, 161, 0}
	for i := byte(128); i < 192; i++ {
		bytes[2] = i
		r, _ := utf8.DecodeRune(bytes)
		runes = append(runes, r)
	}
	return
}()

func (s *Static) Init(start image.Image) {
	s.in = start.Clone()
}

func (s *Static) Update(delta time.Duration) image.Image {
	next := image.New(s.in.Size())
	for row := 0; row < next.Size().R; row++ {
		for col := 0; col < next.Size().C; col++ {
			next[row][col].Char = BRAILLE[rand.Int()%len(BRAILLE)]
		}
	}
	return next
}
