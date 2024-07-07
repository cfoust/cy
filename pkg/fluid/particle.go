package fluid

type Particle struct {
	X       number
	Y       number
	prevX   number
	prevY   number
	velX    number
	velY    number
	springs map[int]number
}

func NewParticle(posX, posY, velX, velY number) Particle {
	return Particle{
		X:     posX,
		Y:     posY,
		prevX: posX,
		prevY: posY,
		velX:  velX,
		velY:  velY,
	}
}

func moveParticleData(dst, src *Particle) {
	dst.X = src.X
	dst.Y = src.Y
	dst.prevX = src.prevX
	dst.prevY = src.prevY
	dst.velX = src.velX
	dst.velY = src.velY
	dst.springs = src.springs
}
