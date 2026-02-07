package main

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/events"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/layout/engine"
	"github.com/cfoust/cy/pkg/layout/prop"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	T "github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/style"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

// MockScreen implements mux.Screen with controllable frame patterns.
type MockScreen struct {
	deadlock.RWMutex
	*mux.UpdatePublisher
	util.Lifetime
	size  geom.Vec2
	frame int
}

var _ mux.Screen = (*MockScreen)(nil)

func NewMockScreen(ctx context.Context) *MockScreen {
	return &MockScreen{
		UpdatePublisher: mux.NewPublisher(),
		Lifetime:        util.NewLifetime(ctx),
		size:            geom.DEFAULT_SIZE,
	}
}

func (m *MockScreen) State() *tty.State {
	m.RLock()
	size := m.size
	frame := m.frame
	m.RUnlock()

	state := tty.New(size)
	pattern := frame % 5
	fillImage(state.Image, pattern, size)
	return state
}

func (m *MockScreen) Subscribe(
	ctx context.Context,
) *util.Subscriber[events.Msg] {
	return m.Publisher.Subscribe(ctx)
}

func (m *MockScreen) Resize(size geom.Vec2) error {
	m.Lock()
	m.size = size
	m.Unlock()
	return nil
}

func (m *MockScreen) Send(msg events.Msg) {}

func (m *MockScreen) Kill() {
	m.Cancel()
}

// Advance increments the frame counter and notifies subscribers.
func (m *MockScreen) Advance() {
	m.Lock()
	m.frame++
	m.Unlock()
	m.Notify()
}

// fillImage fills an image with one of 5 test patterns.
func fillImage(img image.Image, pattern int, size geom.Vec2) {
	switch pattern {
	case 0: // Solid color fill
		color := emu.RGBColor(40, 80, 120)
		for row := 0; row < size.R; row++ {
			for col := 0; col < size.C; col++ {
				img[row][col] = emu.Glyph{
					Char: 'X',
					FG:   color,
					BG:   emu.DefaultBG,
				}
			}
		}

	case 1: // Rainbow gradient
		for row := 0; row < size.R; row++ {
			for col := 0; col < size.C; col++ {
				r := (col * 255) / geom.Max(size.C-1, 1)
				g := (row * 255) / geom.Max(size.R-1, 1)
				b := 128
				img[row][col] = emu.Glyph{
					Char: '#',
					FG:   emu.RGBColor(r, g, b),
					BG:   emu.RGBColor(b, r, g),
				}
			}
		}

	case 2: // Attribute checkerboard
		for row := 0; row < size.R; row++ {
			for col := 0; col < size.C; col++ {
				g := emu.Glyph{
					Char: 'A',
					FG:   emu.ANSIColor(7),
					BG:   emu.ANSIColor(0),
				}
				switch (row + col) % 4 {
				case 0:
					g.Mode = emu.AttrBold
				case 1:
					g.Mode = emu.AttrItalic
				case 2:
					g.Mode = emu.AttrUnderline
				case 3:
					g.Mode = emu.AttrStrikethrough
				}
				img[row][col] = g
			}
		}

	case 3: // Partial update (only ~5 rows differ)
		for row := 0; row < size.R; row++ {
			for col := 0; col < size.C; col++ {
				if row >= 2 && row < 7 {
					img[row][col] = emu.Glyph{
						Char: '~',
						FG:   emu.RGBColor(255, 100, 50),
						BG:   emu.DefaultBG,
					}
				}
				// Leave other rows as EmptyGlyph
			}
		}

	case 4: // 256-color palette
		for row := 0; row < size.R; row++ {
			for col := 0; col < size.C; col++ {
				color := (row*size.C + col) % 256
				img[row][col] = emu.Glyph{
					Char: '.',
					FG:   emu.XTermColor(color),
					BG:   emu.XTermColor(255 - color),
				}
			}
		}
	}
}

// buildLayout constructs a layout tree that exercises multiple node types.
func buildLayout(
	ctx context.Context,
	tree *T.Tree,
	screens []*MockScreen,
) (L.Layout, error) {
	ids := make([]*T.NodeID, len(screens))
	for i, screen := range screens {
		pane := tree.Root().NewPane(ctx, screen)
		id := pane.Id()
		ids[i] = &id
	}

	colorMap := &style.ColorMap{}
	layout := L.New(
		&L.MarginsNode{
			Cols: 80,
			Node: &L.TabsNode{
				Tabs: []L.Tab{
					{
						Active: true,
						Name:   "main",
						Node: &L.SplitNode{
							Vertical: true,
							A: &L.BordersNode{
								Title: prop.NewStatic(
									"left",
								),
								Node: &L.PaneNode{
									Attached: true,
									ID:       ids[0],
								},
							},
							B: &L.SplitNode{
								A: &L.BarNode{
									Text: prop.NewStatic(
										"status",
									),
									Bottom: true,
									Node: &L.PaneNode{
										ID: ids[1],
									},
								},
								B: &L.ColorMapNode{
									Map: prop.NewStatic(
										colorMap,
									),
									Node: &L.PaneNode{
										ID: ids[2],
									},
								},
							},
						},
					},
					{
						Name: "other",
						Node: &L.PaneNode{},
					},
				},
			},
		},
	)

	return layout, nil
}

// BandwidthResult holds bandwidth measurement data.
type BandwidthResult struct {
	TotalBytes    int64   `json:"total_bytes"`
	FrameCount    int     `json:"frame_count"`
	BytesPerFrame float64 `json:"bytes_per_frame"`
	MBPerSecond   float64 `json:"mb_per_second"`
}

// Result holds the benchmark results.
type Result struct {
	CompositeStats Statistics      `json:"composite_stats"`
	SwapStats      Statistics      `json:"swap_stats"`
	Bandwidth      BandwidthResult `json:"bandwidth"`
}

// Config holds the benchmark configuration.
type Config struct {
	Size   geom.Size
	Frames int
	Warmup int
}

// Report holds the complete benchmark report.
type Report struct {
	Timestamp time.Time
	Config    Config
	Result    Result
}

func runBenchmark(config Config) (Result, error) {
	ctx := context.Background()
	tree := T.NewTree()
	srv := server.New()

	// Create 3 mock screens
	screens := make([]*MockScreen, 3)
	for i := range screens {
		screens[i] = NewMockScreen(ctx)
	}

	// Build layout
	layout, err := buildLayout(ctx, tree, screens)
	if err != nil {
		return Result{}, err
	}

	// Create engine
	eng := engine.New(ctx, tree, srv)
	if err := eng.Set(layout); err != nil {
		return Result{}, err
	}
	if err := eng.Resize(config.Size); err != nil {
		return Result{}, err
	}

	// Let the engine settle
	time.Sleep(50 * time.Millisecond)

	// Initialize previous state
	prevState := tty.New(config.Size)

	advanceAll := func() {
		for _, s := range screens {
			s.Advance()
		}
	}

	// Warmup
	for i := 0; i < config.Warmup; i++ {
		advanceAll()
		state := eng.State()
		tty.Swap(prevState, state)
		prevState = state
	}

	// Measured frames
	compositeDurations := make([]time.Duration, config.Frames)
	swapDurations := make([]time.Duration, config.Frames)
	var totalSwapBytes int64

	for i := 0; i < config.Frames; i++ {
		advanceAll()

		start := time.Now()
		state := eng.State()
		compositeDurations[i] = time.Since(start)

		start = time.Now()
		swapBytes := tty.Swap(prevState, state)
		swapDurations[i] = time.Since(start)

		totalSwapBytes += int64(len(swapBytes))
		prevState = state
	}

	// Calculate statistics
	compositeStats := calculateStatistics(compositeDurations)
	swapStats := calculateStatistics(swapDurations)

	// Calculate bandwidth
	totalTimeMs := compositeStats.TotalMs + swapStats.TotalMs
	mbPerSecond := 0.0
	if totalTimeMs > 0 {
		mbPerSecond = float64(totalSwapBytes) /
			(1024 * 1024) /
			(totalTimeMs / 1000.0)
	}

	// Clean up
	eng.Kill()
	for _, s := range screens {
		s.Kill()
	}

	return Result{
		CompositeStats: compositeStats,
		SwapStats:      swapStats,
		Bandwidth: BandwidthResult{
			TotalBytes:    totalSwapBytes,
			FrameCount:    config.Frames,
			BytesPerFrame: float64(totalSwapBytes) / float64(config.Frames),
			MBPerSecond:   mbPerSecond,
		},
	}, nil
}
