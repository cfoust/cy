package frames

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
)

func getSize(text string) geom.Vec2 {
	return geom.Vec2{
		R: lipgloss.Height(text),
		C: lipgloss.Width(text),
	}
}

func renderSignature(state image.Image, signature string) {
	size := state.Size()
	render := taro.NewRenderer()
	signatureSize := getSize(signature)
	render.RenderAt(
		state,
		size.R-signatureSize.R,
		size.C-1-signatureSize.C,
		signature,
	)
}

func createPattern(pattern, signature string, offsetR, offsetC int) Frame {
	// cut out the newline
	pattern = pattern[1:]
	patternSize := getSize(pattern)
	render := taro.NewRenderer()

	puzzleSize := getSize(pattern)
	brush := image.New(puzzleSize)
	render.RenderAt(brush, 0, 0, pattern)

	return func(state image.Image) {
		size := state.Size()
		for row := offsetR; row < size.R; row += patternSize.R + offsetR {
			for col := offsetC; col < size.C; col += patternSize.C + offsetC {
				image.Compose(
					geom.Vec2{
						R: row,
						C: col,
					},
					state,
					brush,
				)
			}
		}

		if len(signature) == 0 {
			return
		}

		renderSignature(state, signature)
	}
}

// Some (but not all) of these patterns are from:
// https://www.asciiart.eu/art-and-design/patterns
// In accordance with the plea on the site's front page, signatures are
// included as they appeared.

var HiveThick = createPattern(`
 \__/
_/  \_`, "", 0, 0)

var Zigzag = createPattern(`
\\//
//\\`, "", 0, 0)

var CrossStitch = createPattern(`
\/
<>
/\
==
<>
==`, "gfj/98", 0, 0)

var Wallpaper = createPattern(`
/|/ \|\ `, "PS", 0, 0)

var BigHex = createPattern(`
     /  \
__/        \_
  \        /
     \__/    `, "", 0, 0)

var Puzzle = createPattern(`
     _
   _( )_|
 _|     _|
(_   _ (_
 |__( )_|_
 |_     |_
  _) _   _
 |__( )_|_
`, "mx", -3, -3)

var Brick = createPattern(`
|___|___|___
__|___|___|_`, "", 0, 0)

var Hive = createPattern(`
/ \
\_/`, "", 0, 0)

var Cross = createPattern(`
 X
/ \
\ /`, "", 0, 0)

// TODO(cfoust): 11/17/23 too lazy to fix this
var Squares = createPattern(`
 __|__|
|   __|
|__|`, "", -1, -3)

var Tiles = createPattern(`
\  \--
 \__\
__\  \
\  \__
 \__\
__\  \`, "", 0, 0)

var Stars = createPattern(`
_/\_
/ _/
\/ \
/ _/`, "", 0, 0)

var Cheerios = createPattern(`
 / __ \
/ /  \ \
\ \__/ /
 \____/`, "", 0, 0)

var DotBricks = createPattern(`
---+---+---+---+
 o | o   o | o  `, "", 0, 0)

func init() {
	registerFrame("big-hex", BigHex)
	registerFrame("brick", Brick)
	registerFrame("cheerios", Cheerios)
	registerFrame("cross", Cross)
	registerFrame("cross-stitch", CrossStitch)
	registerFrame("dot-bricks", DotBricks)
	registerFrame("hive", Hive)
	registerFrame("hive-thick", HiveThick)
	registerFrame("puzzle", Puzzle)
	registerFrame("squares", Squares)
	registerFrame("stars", Stars)
	registerFrame("tiles", Tiles)
	registerFrame("wallpaper", Wallpaper)
	registerFrame("zigzag", Zigzag)
}
