package search

import (
	"regexp"
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"

	"github.com/stretchr/testify/require"
	"github.com/xo/terminfo"
)

var TEST_SIZE = geom.Size{
	R: 4,
	C: 10,
}

func TestBasic(t *testing.T) {
	sim := sessions.NewSimulator().
		Add(
			"foo",
			"bar",
			"baz",
		)
	results, err := Search(sim.Events(), "bar", nil)
	require.NoError(t, err)
	require.Equal(t, 1, len(results))
	require.Equal(t, SearchResult{
		Begin: Address{
			Index:  1,
			Offset: 2,
		},
		End: Address{
			Index:  2,
			Offset: 2,
		},
		Appearances: []Appearance{
			{
				Begin: Address{
					Index:  1,
					Offset: 2,
				},
				End: Address{
					Index:  2,
					Offset: 2,
				},
				Selection: Selection{
					From: geom.Vec2{
						C: 3,
					},
					To: geom.Vec2{
						C: 5,
					},
				},
			},
		},
	}, results[0])
}

func TestClearScreen(t *testing.T) {
	sim := sessions.NewSimulator().
		Add(
			TEST_SIZE,
			emu.LineFeedMode,
			"foo\n",
			"bar\n",
			"baz",
		).
		Term(terminfo.ClearScreen).
		Add("test")

	results, err := Search(sim.Events(), "foo", nil)
	require.NoError(t, err)
	require.Equal(t, 1, len(results))
	require.Equal(t, SearchResult{
		Begin: Address{
			Index:  2,
			Offset: 2,
		},
		End: Address{
			Index:  5,
			Offset: 6,
		},
		Appearances: []Appearance{
			{
				Begin: Address{
					Index:  2,
					Offset: 2,
				},
				End: Address{
					Index:  5,
					Offset: 6,
				},
				Selection: Selection{
					From: geom.Vec2{
						C: 0,
					},
					To: geom.Vec2{
						C: 2,
					},
				},
			},
		},
	}, results[0])
}

func TestClearLine(t *testing.T) {
	sim := sessions.NewSimulator().
		Add(
			TEST_SIZE,
			emu.LineFeedMode,
			"foo\n",
			"bar\n",
			"baz",
		).
		Term(terminfo.CursorAddress, 0, 0).
		Term(terminfo.ClrEol).
		Add("baz")

	results, err := Search(sim.Events(), "foo", nil)
	require.NoError(t, err)
	require.Equal(t, 1, len(results))
	require.Equal(t, SearchResult{
		Begin: Address{
			Index:  2,
			Offset: 2,
		},
		End: Address{
			Index:  6,
			Offset: 2,
		},
		Appearances: []Appearance{
			{
				Begin: Address{
					Index:  2,
					Offset: 2,
				},
				End: Address{
					Index:  6,
					Offset: 2,
				},
				Selection: Selection{
					From: geom.Vec2{
						C: 0,
					},
					To: geom.Vec2{
						C: 2,
					},
				},
			},
		},
	}, results[0])
}

func TestScroll(t *testing.T) {
	sim := sessions.NewSimulator().
		Add(
			TEST_SIZE,
			emu.LineFeedMode,
			"foo\n",
			"bar\n", // 3
			"baz\n",
			"\n",
			"\n", // 6
			"test",
		)

	results, err := Search(sim.Events(), "bar", nil)
	require.NoError(t, err)
	require.Equal(t, 1, len(results))
	require.Equal(t, SearchResult{
		Begin: Address{
			Index:  3,
			Offset: 2,
		},
		End: Address{
			Index:  7,
			Offset: 3,
		},
		Appearances: []Appearance{
			{
				Begin: Address{
					Index:  3,
					Offset: 2,
				},
				End: Address{
					Index:  5,
					Offset: 0,
				},
				Selection: Selection{
					From: geom.Vec2{
						R: 1,
						C: 0,
					},
					To: geom.Vec2{
						R: 1,
						C: 2,
					},
				},
			},
			{
				Begin: Address{
					Index:  5,
					Offset: 0,
				},
				End: Address{
					Index:  7,
					Offset: 3,
				},
				Selection: Selection{
					From: geom.Vec2{
						R: 0,
						C: 0,
					},
					To: geom.Vec2{
						R: 0,
						C: 2,
					},
				},
			},
		},
	}, results[0])
}

func TestPrinted(t *testing.T) {
	sim := sessions.NewSimulator().
		Add(
			TEST_SIZE,
			emu.LineFeedMode,
			"foo\n", // 2
			"bar\n",
		).
		Term(terminfo.CursorAddress, 0, 1). // 4
		Add(
			"a",
		)

	results, err := Search(sim.Events(), "foo", nil)
	require.NoError(t, err)
	require.Equal(t, 1, len(results))
	require.Equal(t, SearchResult{
		Begin: Address{
			Index:  2,
			Offset: 2,
		},
		End: Address{
			Index:  5,
			Offset: 0,
		},
		Appearances: []Appearance{
			{
				Begin: Address{
					Index:  2,
					Offset: 2,
				},
				End: Address{
					Index:  5,
					Offset: 0,
				},
				Selection: Selection{
					From: geom.Vec2{
						R: 0,
						C: 0,
					},
					To: geom.Vec2{
						R: 0,
						C: 2,
					},
				},
			},
		},
	}, results[0])
}

func TestSearcher(t *testing.T) {
	sim := sessions.NewSimulator().
		Add("foo").
		Add("f").
		Add(geom.DEFAULT_SIZE).
		Add("oo").
		Add("bar").
		Add("foo f").
		Term(terminfo.ClearScreen).
		Add("oo")

	pattern, _ := regexp.Compile("foo")
	s := NewSearcher()
	s.Parse(sim.Events())

	matches := s.Find(pattern)
	require.Equal(t, 4, len(matches))
	require.Equal(t, []Match{
		{
			Begin: Address{
				Index:  0,
				Offset: 0,
			},
			End: Address{
				Index:  0,
				Offset: 3,
			},
			Continuous: true,
		},
		{
			Begin: Address{
				Index:  1,
				Offset: 0,
			},
			End: Address{
				Index:  3,
				Offset: 2,
			},
			Continuous: true,
		},
		{
			Begin: Address{
				Index:  5,
				Offset: 0,
			},
			End: Address{
				Index:  5,
				Offset: 3,
			},
			Continuous: true,
		},
		{
			Begin: Address{
				Index:  5,
				Offset: 4,
			},
			End: Address{
				Index:  7,
				Offset: 2,
			},
			Continuous: false,
		},
	}, matches)
}
