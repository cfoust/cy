package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/cfoust/cy/cmd/stories/overlay"
	"github.com/cfoust/cy/pkg/anim"
	_ "github.com/cfoust/cy/pkg/cy"
	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/stream/cli"
	"github.com/cfoust/cy/pkg/mux/stream/renderer"
	_ "github.com/cfoust/cy/pkg/rasterion/stories"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/stories"
	"github.com/cfoust/cy/pkg/stories/ui"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/alecthomas/kong"
	"github.com/rs/zerolog/log"
	"github.com/xo/terminfo"
	"golang.org/x/term"
)

var CLI struct {
	Prefix string `help:"Pre-filter the list of stories." name:"prefix" optional:"" short:"p"`
	Single string `help:"Show a single story in full screen, overriding its config." name:"single" optional:"" short:"s"`
	Cast   string `help:"Save an asciinema cast to the given filename. Must be used in tandem with --single." name:"cast" optional:"" short:"c"`
	Width  int    `help:"Set the width of the terminal when saving an asciinema cast." name:"width" optional:"" short:"w" default:"80"`
	Height int    `help:"Set the height of the terminal when saving an asciinema cast." name:"height" optional:"" short:"h" default:"26"`

	Overlay      bool    `help:"Overlay cursor from cast onto a GIF." name:"overlay" optional:""`
	ExtractFrame bool    `help:"Extract the last frame of a GIF as PNG." name:"extract-frame" optional:""`
	GIF          string  `help:"Input GIF file (for --overlay or --extract-frame)." name:"gif" optional:""`
	Output       string  `help:"Output file path." name:"output" optional:"" short:"o"`
	FontSize     float64 `help:"Font size used by agg (for cursor mapping)." name:"font-size" optional:"" default:"18"`
	LineHeight   float64 `help:"Line height used by agg (for cursor mapping)." name:"line-height" optional:"" default:"1.4"`
	Speed        float64 `help:"Playback speed used by agg (for cursor timing)." name:"speed" optional:"" default:"0.5"`
}

func main() {
	kong.Parse(&CLI,
		kong.Name("cy-stories"),
		kong.Description("storybook, but for the CLI"),
		kong.UsageOnError(),
		kong.ConfigureHelp(kong.HelpOptions{
			Compact: true,
			Summary: true,
		}))

	if CLI.Overlay {
		if CLI.Cast == "" || CLI.GIF == "" || CLI.Output == "" {
			panic("--overlay requires --cast, --gif, and --output")
		}
		err := overlay.GIF(
			CLI.Cast, CLI.GIF, CLI.Output,
			CLI.FontSize, CLI.LineHeight, CLI.Speed,
		)
		if err != nil {
			panic(err)
		}
		return
	}

	if CLI.ExtractFrame {
		if CLI.GIF == "" || CLI.Output == "" {
			panic("--extract-frame requires --gif and --output")
		}
		err := extractLastFrame(CLI.GIF, CLI.Output)
		if err != nil {
			panic(err)
		}
		return
	}

	logs, err := os.OpenFile(
		"stories.log",
		os.O_CREATE|os.O_APPEND|os.O_WRONLY,
		0644,
	)
	if err != nil {
		panic(err)
	}
	log.Logger = log.Output(logs)

	for name, frame := range frames.Frames {
		func(f frames.Frame) {
			stories.Register(
				fmt.Sprintf("frame/%s", name),
				func(ctx context.Context) (mux.Screen, error) {
					framer := frames.NewFramer(
						ctx,
						f,
					)
					return framer, nil
				},
				stories.Config{},
			)
		}(frame)
	}

	for name, animation := range anim.Animations {
		func(a anim.Creator) {
			stories.Register(
				fmt.Sprintf("animation/%s", name),
				func(ctx context.Context) (mux.Screen, error) {
					return NewAnimationStory(
						ctx,
						a,
					), nil
				},
				stories.Config{},
			)
		}(animation)
	}

	// A stories story
	stories.Register(
		"stories",
		func(ctx context.Context) (mux.Screen, error) {
			return ui.New(ctx, CLI.Prefix)
		},
		stories.Config{
			Input: []interface{}{
				stories.Type("ctrl+j"),
				stories.Wait(stories.Some),
				stories.Type("ctrl+j"),
				stories.Wait(stories.Some),
				stories.Type("ctrl+k"),
				stories.Wait(stories.Some),
				stories.Type("ctrl+k"),
				stories.Wait(stories.Some),
				stories.Type("input"),
				stories.Type("ctrl+j"),
				stories.Wait(stories.Some),
				stories.Type("ctrl+j"),
				stories.Wait(stories.Some),
				stories.Type("ctrl+k"),
				stories.Wait(stories.Some),
				stories.Type("ctrl+k"),
				stories.Wait(stories.Some),
			},
		},
	)

	haveCast := len(CLI.Cast) > 0
	if len(CLI.Single) == 0 && haveCast {
		panic(fmt.Errorf("to use --cast, you must provide a single story"))
	}

	info, err := terminfo.LoadFromEnv()
	if err != nil {
		panic(err)
	}

	ctx := context.Background()

	var (
		screen   *taro.Program
		recorder *sessions.MemoryRecorder
	)

	if haveCast {
		recorder = sessions.NewMemoryRecorder()
	}

	if len(CLI.Single) > 0 {
		story, ok := stories.Stories[CLI.Single]
		if !ok {
			panic(fmt.Errorf("story %s not found", CLI.Single))
		}

		var opts []ui.ViewerOption
		if recorder != nil {
			opts = append(opts, ui.WithCursorHandler(
				func(r, c int, drag bool) {
					_ = recorder.Process(sessions.Event{
						Stamp: time.Now(),
						Message: P.CursorMessage{
							Row:    r,
							Column: c,
							Drag:   drag,
						},
					})
				},
			))
		}

		screen, err = ui.NewViewer(
			ctx,
			story,
			!haveCast,
			opts...,
		)
		if err != nil {
			panic(err)
		}

		if haveCast && !story.Config.HasInputs() {
			go func() {
				time.Sleep(5 * time.Second)
				screen.Cancel()
			}()
		}
	} else {
		screen, err = ui.New(ctx, CLI.Prefix)
		if err != nil {
			panic(err)
		}
	}

	var size geom.Size
	if haveCast {
		size = geom.Size{C: CLI.Width, R: CLI.Height}
		if len(CLI.Single) > 0 {
			story := stories.Stories[CLI.Single]
			if !story.Config.Size.IsZero() {
				size = story.Config.Size
			}
		}
	} else {
		cols, rows, sizeErr := term.GetSize(
			int(os.Stdin.Fd()),
		)
		if sizeErr != nil {
			cols, rows = 80, 26
		}
		size = geom.Size{C: cols, R: rows}
	}

	renderer := renderer.NewRenderer(screen.Ctx(), info, size, screen)

	if !haveCast {
		_ = cli.Attach(screen.Ctx(), renderer, os.Stdin, os.Stdout)
		return
	}

	stream := sessions.NewEventStream(renderer, recorder)

	// The recorder only stores data on Read() calls, so we need to
	// drain it
	go func() { _, _ = io.Copy(io.Discard, stream) }()
	_ = stream.Resize(size)
	<-screen.Ctx().Done()

	err = sessions.WriteAsciinema(
		CLI.Cast,
		size.C, size.R,
		recorder.Events(),
	)
	if err != nil {
		panic(err)
	}
}
