package thumbs

import (
	"context"
	"fmt"
	"regexp"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/stories"
)

func createThumbStory(
	ctx context.Context,
	content string,
	patterns []*regexp.Regexp,
) (mux.Screen, error) {
	t := emu.New()
	t.Write([]byte(emu.LineFeedMode))
	t.Write([]byte(content))

	var (
		i    = image.Capture(t)
		size = i.Size()
	)

	// Remove regions
	for row := range size.R {
		for col := range size.C {
			i[row][col].Write = 0
		}
	}

	matches := Find(patterns, i)
	if len(matches) == 0 {
		return nil, fmt.Errorf(
			"no matches",
		)
	}

	return New(
		ctx,
		i,
		geom.Vec2{
			R: geom.DEFAULT_SIZE.R - 1,
			C: geom.DEFAULT_SIZE.C / 2,
		},
		matches,
	), nil
}

func init() {
	stories.Register(
		"input/thumbs/basic",
		func(ctx context.Context) (mux.Screen, error) {
			content := `Visit https://github.com/cfoust/cy for more info

File paths: /var/log/nginx.log and /tmp/test.txt
IP addresses: 127.0.0.1 and 192.168.1.100
Git SHA: a1b2c3d4e5f6 and full hash 973113963b491874ab2e372ee60d4b4cb75f717c
UUID: 123e4567-e89b-12d3-a456-426655440000
Colors: #ff0000, #00ff00, #0000ff
Docker: sha256:30557a29d5abc51e5f1d5b472e79b7e296f595abcf19fe6b9199dbbc809c6ff4
Numbers: 12345, 678910, 99999, 12345
Numbers: 12345, 678910, 99999, 12345
Numbers: 12345, 678910, 99999, 12345
Numbers: 12345, 678910, 99999, 12345
`

			return createThumbStory(
				ctx,
				content,
				CompiledDefaultPatterns,
			)
		},
		stories.Config{
			Size: geom.DEFAULT_SIZE,
			Input: []any{
				stories.Wait(stories.Some),
				stories.Type("a"),
				stories.Wait(stories.Some),
			},
		},
	)
}
