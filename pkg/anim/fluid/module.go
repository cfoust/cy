package fluid

type number = float64

type Simulator struct {
	width               number
	height              number
	particles           []Particle

	mouseX     number
	mouseY     number
	mousePrevX number
	mousePrevY number
	attract    bool
	numHashBuckets   int
	numActiveBuckets int
	activeBuckets    []int
	// Same size as numHashBuckets, each points to first particle in bucket list
	particleListHeads   []int
	particleListNextIdx []int

	material Material
}

func (s *Simulator) Particles() []Particle {
	return s.particles
}

func New(width, height number, particles []Particle) *Simulator {
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
	return s
}
