package thumbs

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/sessions/search"

	"github.com/stretchr/testify/require"
)

func setWrite(
	i image.Image,
	write int,
	fromRow, fromCol, toRow, toCol int,
) {
	for col := fromCol; col < toCol; col++ {
		for row := fromRow; row < toRow; row++ {
			i[row][col].Write = emu.WriteID(write)
		}
	}
}

func TestRegions(t *testing.T) {
	size := geom.Vec2{
		R: 8,
		C: 8,
	}
	i := image.New(size)

	for count := range 2 {
		col := count * 4
		setWrite(i, count+1, 0, col, 8, col+4)
	}

	require.Equal(t,
		[]search.Selection{
			{
				To: size,
			},
			{
				From: geom.Vec2{R: 0, C: 0},
				To:   geom.Vec2{R: 8, C: 4},
			},
			{
				From: geom.Vec2{R: 0, C: 4},
				To:   geom.Vec2{R: 8, C: 8},
			},
		},
		getRegions(i),
	)
}
