package fluid

import (
	"math/rand"
)

type number = float64

type Simulator struct {
	running bool

	width               number
	height              number
	particles           []Particle
	screenX             number
	screenY             number
	screenMoveSmootherX number
	screenMoveSmootherY number

	mouseX     number
	mouseY     number
	mousePrevX number
	mousePrevY number
	attract    bool
	repel      bool
	emit       bool
	drain      bool
	drag       bool

	useSpatialHash   bool
	numHashBuckets   int
	numActiveBuckets int
	activeBuckets    []int
	// Same size as numHashBuckets, each points to first particle in bucket list
	particleListHeads   []int
	particleListNextIdx []int

	material Material
}

func New(width, height number, numParticles int) *Simulator {
	s := &Simulator{
		width:          width,
		height:         height,
		mouseX:         width / 2,
		mouseY:         height / 2,
		numHashBuckets: 5000,
	}

	s.mousePrevX = s.mouseX
	s.mousePrevY = s.mouseY

	s.particles = make([]Particle, numParticles)
	for i := 0; i < numParticles; i++ {
		posX := rand.Float64() * width
		posY := rand.Float64() * height
		velX := rand.Float64()*2 - 1
		velY := rand.Float64()*2 - 1
		s.particles[i] = NewParticle(posX, posY, velX, velY)
	}

	s.particleListHeads = make([]int, s.numHashBuckets)
	s.activeBuckets = make([]int, s.numHashBuckets)
	for i := 0; i < s.numHashBuckets; i++ {
		s.particleListHeads[i] = -1
		s.activeBuckets[i] = 0
	}

	s.particleListNextIdx = make([]int, numParticles)

	s.material = NewMaterial("water", 4, 0.5, 0.5, 40)
	return s
}
