package cy

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/replay"
	"github.com/cfoust/cy/pkg/replay/player"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/stories"
)

const (
	REPLAY_PROMPT = "\033Pcy\033\\\033[0;31m▸▸cy▹\033[0m \033[0;31m\033[0m\033[00m"
)

var initReplay stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	server, client, screen, err := createStory(ctx)

	// All filler text here is courtesy Wikipedia.
	events := sessions.NewSimulator().
		Add(
			emu.LineFeedMode,
			geom.DEFAULT_SIZE,
		).
		AddTime(
			time.Second/4,
			REPLAY_PROMPT,
		).
		AddTime(time.Second/4, "summarize tolstoy").
		AddTime(time.Second/4, "\n").
		AddTime(
			time.Second/4,
			`Born into an aristocratic family, Tolstoy's notable works include the novels War and Peace (1869) and Anna Karenina (1878),[5] often cited as pinnacles of realist fiction,[2] and two of the greatest books of all time.[3][4] He first achieved literary acclaim in his twenties with his semi-autobiographical trilogy, Childhood, Boyhood, and Youth (1852–1856), and Sevastopol Sketches (1855), based upon his experiences in the Crimean War. His fiction includes dozens of short stories such as "After the Ball" (1911), and several novellas such as The Death of Ivan Ilyich (1886), Family Happiness (1859) and Hadji Murad (1912). He also wrote plays and essays concerning philosophical, moral and religious themes.`,
		).
		Add("\n").
		AddTime(
			time.Second/4,
			REPLAY_PROMPT,
		).
		AddTime(time.Second/4, "summarize cao xueqin").
		AddTime(time.Second/4, "\n").
		AddTime(
			time.Second/4,
			`Cao Xueqin was born to a Han Chinese clan[5] that was brought into personal service (as booi aha or bondservants of Cigu Niru) to the Manchu royalty in the late 1610s.[6] His ancestors distinguished themselves through military service in the Plain White Banner of the Eight Banners and subsequently held posts as officials which brought both prestige and wealth.`,
		).
		Add("\n").
		AddTime(
			time.Second/4,
			REPLAY_PROMPT,
		).
		AddTime(time.Second/4, "summarize marshall mcluhan").
		AddTime(time.Second/4, "\n").
		AddTime(
			time.Second/4,
			`McLuhan was born on July 21, 1911, in Edmonton, Alberta, and was named "Marshall" from his maternal grandmother's surname. His brother, Maurice, was born two years later. His parents were both also born in Canada: his mother, Elsie Naomi (née Hall), was a Baptist school teacher who later became an actress; and his father, Herbert Ernest McLuhan, was a Methodist with a real-estate business in Edmonton. When the business failed at the start of World War I, McLuhan's father enlisted in the Canadian Army. After a year of service, he contracted influenza and remained in Canada, away from the front lines. After Herbert's discharge from the army in 1915, the McLuhan family moved to Winnipeg, Manitoba, where Marshall grew up and went to school, attending Kelvin Technical School before enrolling in the University of Manitoba in 1928.`,
		).
		Add("\n").
		AddTime(
			time.Second/4,
			REPLAY_PROMPT,
		).
		Events()

	player := player.FromEvents(events)

	r := replay.New(
		ctx,
		player,
		server.timeBinds,
		server.copyBinds,
	)
	if err != nil {
		return nil, err
	}

	// Create a sample screen to attach to
	p := server.tree.Root().NewPane(ctx, r)

	_ = client.Attach(p)

	return screen, err
}
