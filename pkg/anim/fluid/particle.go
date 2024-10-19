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
