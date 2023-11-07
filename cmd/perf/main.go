package main

import (
	"fmt"
	"os"
	"runtime/pprof"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/sessions/search"
)

const (
	B  uint64 = 1
	KB        = B << 10
	MB        = KB << 10
	GB        = MB << 10
)

func main() {
	sim := sessions.NewSimulator()
	sim.Add(
		"\033[20h", // CRLF -- why is this everywhere?
		geom.DEFAULT_SIZE,
	)

	var numBytes, desiredBytes uint64
	desiredBytes = 2 * MB
	i := 0
	numMatches := 0
	for numBytes < desiredBytes {
		s := "foo"
		if (i % 100) == 0 {
			numMatches++
			s = "bar"
		}
		sim.Add(s)
		numBytes += uint64(len([]byte(s)))
		i++
	}

	fmt.Printf("%d matches in %d bytes", numMatches, desiredBytes)

	f, err := os.Create("search.prof")
	if err != nil {
		panic(err)
	}
	defer f.Close()
	if err := pprof.StartCPUProfile(f); err != nil {
		panic(err)
	}
	defer pprof.StopCPUProfile()

	_, err = search.Search(sim.Events(), "bar")
	if err != nil {
		panic(err)
	}
}
