package fluid

type Particle struct {
	posX    number
	posY    number
	prevX   number
	prevY   number
	velX    number
	velY    number
	springs []number
}

func NewParticle(posX, posY, velX, velY number) Particle {
	return Particle{
		posX:  posX,
		posY:  posY,
		prevX: posX,
		prevY: posY,
		velX:  velX,
		velY:  velY,
	}
}

func moveParticleData(dst, src *Particle) {
	dst.posX = src.posX
	dst.posY = src.posY
	dst.prevX = src.prevX
	dst.prevY = src.prevY
	dst.velX = src.velX
	dst.velY = src.velY
	dst.springs = src.springs
}
