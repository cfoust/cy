package cy

import (
	"context"
	_ "embed"

	"github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/layout/prop"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/replay"
	"github.com/cfoust/cy/pkg/replay/player"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/stories"
	"github.com/cfoust/cy/pkg/style"
)

//go:embed theme_story.janet
var THEME_STORY_SETUP string

var initTheme stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	server, client, screen, err := createStory(ctx)
	if err != nil {
		return nil, err
	}

	err = client.execute(THEME_STORY_SETUP)
	if err != nil {
		return nil, err
	}

	rootParams := server.tree.Root().Params()
	s := sessions.NewSimulator().
		Defaults().
		Add(
			"This sense of wandering in a space which is at once informational and uses the open Internet as a guiding principle for navigation is something that builds on a series of \"Net Nomad\" projects developed by the artist in the 1990s. Here, Cheang launches herself into the space of the nets, roaming the world and documenting her travels, finding routes and the means to travel via blagged and hustled connections, throwing life through the window onto the screen.\n\n",
			"3x3x6, shu lea chang",
		)

	var replayTop int32
	{
		r := replay.New(
			ctx,
			player.FromEvents(s.Events()),
			server.timeBinds,
			server.copyBinds,
			replay.WithParams(rootParams),
			replay.WithCopyMode,
		)
		if err != nil {
			return nil, err
		}
		p := server.tree.Root().NewPane(ctx, r)
		replayTop = p.Id()
	}

	var replayBottom int32
	{
		r := replay.New(
			ctx,
			player.FromEvents(s.Events()),
			server.timeBinds,
			server.copyBinds,
			replay.WithParams(rootParams),
		)
		if err != nil {
			return nil, err
		}
		p := server.tree.Root().NewPane(ctx, r)
		replayBottom = p.Id()
	}

	err = client.SetLayout(layout.New(layout.SplitType{
		Vertical: true,
		Border:   prop.NewStatic(&style.DefaultBorder),
		A: layout.PaneType{
			ID: &replayTop,
		},
		B: layout.PaneType{
			ID:       &replayBottom,
			Attached: true,
		},
	}))
	if err != nil {
		return nil, err
	}

	err = client.execute(`
(msg/toast :info "this shows up in blue")
(msg/toast :warn "this shows up in yellow")
(msg/toast :error "this shows up in red")
		`)
	if err != nil {
		return nil, err
	}

	go func() {
		client.execute(`
(input/find @["У лукоморья дуб зелёный" "Златая цепь на дубе том" "И днём и ночью кот учёный"]
        :prompt "выбор строчки"
	:animated false)`)
	}()

	return screen, err
}
