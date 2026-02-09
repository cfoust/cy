package fluid

import "math/rand"

type number = float64

// Option configures a Simulator.
type Option func(*Simulator)

// WithDrain adds a hole in the bottom and top boundaries at random X
// positions. Particles that fall through the bottom hole wrap to the top.
func WithDrain() Option {
	return func(s *Simulator) {
		holeWidth := s.width / 4
		maxOffset := s.width - holeWidth
		bottomStart := rand.Float64() * maxOffset
		topStart := rand.Float64() * maxOffset
		s.drainEnabled = true
		s.bottomHoleMinX = bottomStart
		s.bottomHoleMaxX = bottomStart + holeWidth
		s.topHoleMinX = topStart
		s.topHoleMaxX = topStart + holeWidth
	}
}

type Simulator struct {
	width     number
	height    number
	particles []Particle

	mouseX           number
	mouseY           number
	mousePrevX       number
	mousePrevY       number
	attract          bool
	numHashBuckets   int
	numActiveBuckets int
	activeBuckets    []int
	// Same size as numHashBuckets, each points to first particle in bucket list
	particleListHeads   []int
	particleListNextIdx []int

	material Material

	// Drain mode: particles in the bottom hole wrap to the top hole
	drainEnabled   bool
	bottomHoleMinX number
	bottomHoleMaxX number
	topHoleMinX    number
	topHoleMaxX    number
}

func (s *Simulator) Particles() []Particle {
	return s.particles
}

func New(
	width, height number,
	particles []Particle,
	options ...Option,
) *Simulator {
	s := &Simulator{
		width:          width,
		height:         height,
		mouseX:         width / 2,
		mouseY:         height / 2,
		numHashBuckets: 5000,
	}

	s.mousePrevX = s.mouseX
	s.mousePrevY = s.mouseY

	s.particles = particles

	s.particleListHeads = make([]int, s.numHashBuckets)
	s.activeBuckets = make([]int, s.numHashBuckets)
	for i := 0; i < s.numHashBuckets; i++ {
		s.particleListHeads[i] = -1
		s.activeBuckets[i] = 0
	}

	s.particleListNextIdx = make([]int, len(particles))

	s.material = NewMaterial("water", 4, 0.5, 0.5, 40)

	for _, opt := range options {
		opt(s)
	}

	return s
}
