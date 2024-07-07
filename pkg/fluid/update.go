package fluid

import (
	"math"
)

// Mueller 10 minute physics
func (s *Simulator) getHashBucketIdx(bucketX, bucketY int) int {
	h := ((bucketX * 92837111) ^ (bucketY * 689287499))
	return int(math.Abs(float64(h))) % s.numHashBuckets
}

func (s *Simulator) populateHashGrid() {
	for i := 0; i < s.numActiveBuckets; i++ {
		s.particleListHeads[s.activeBuckets[i]] = -1
	}

	for i := 0; i < s.numHashBuckets; i++ {
		s.particleListHeads[i] = -1
	}

	s.numActiveBuckets = 0

	// Populate the hash grid
	numParticles := len(s.particles)
	bucketSize := s.material.kernelRadius // Same as kernel radiu
	bucketSizeInv := 1.0 / bucketSize

	for i := 0; i < numParticles; i++ {
		bucketX := int(math.Floor(s.particles[i].posX * bucketSizeInv))
		bucketY := int(math.Floor(s.particles[i].posY * bucketSizeInv))
		bucketIdx := s.getHashBucketIdx(bucketX, bucketY)
		headIdx := s.particleListHeads[bucketIdx]

		if headIdx == -1 {
			s.activeBuckets[s.numActiveBuckets] = bucketIdx
			s.numActiveBuckets++
		}

		s.particleListNextIdx[i] = headIdx
		s.particleListHeads[bucketIdx] = i
	}
}

func (s *Simulator) applyViscosity(dt number) {
	if s.material.linViscosity == 0 && s.material.quadViscosity == 0 {
		return
	}

	numActiveBuckets := s.numActiveBuckets
	var visitedBuckets []int

	kernelRadius := s.material.kernelRadius // h
	kernelRadiusSq := kernelRadius * kernelRadius
	kernelRadiusInv := 1.0 / kernelRadius

	linViscosity := s.material.linViscosity * dt
	quadViscosity := s.material.quadViscosity * dt

	for abIdx := 0; abIdx < numActiveBuckets; abIdx++ {
		selfIdx := s.particleListHeads[s.activeBuckets[abIdx]]

		for selfIdx != -1 {
			p0 := s.particles[selfIdx]
			numVisitedBuckets := 0

			// Compute density and near-density
			bucketX := math.Floor(p0.posX * kernelRadiusInv)
			bucketY := math.Floor(p0.posY * kernelRadiusInv)

			for bucketDX := -1; bucketDX <= 1; bucketDX++ {
				for bucketDY := -1; bucketDY <= 1; bucketDY++ {
					bucketIdx := s.getHashBucketIdx(
						int(math.Floor(bucketX+float64(bucketDX))),
						int(math.Floor(bucketY+float64(bucketDY))),
					)

					// Check hash collision
					found := false
					for k := 0; k < numVisitedBuckets; k++ {
						if visitedBuckets[k] == bucketIdx {
							found = true
							break
						}
					}

					if found {
						continue
					}

					visitedBuckets[numVisitedBuckets] = bucketIdx
					numVisitedBuckets++

					neighborIdx := s.particleListHeads[bucketIdx]

					for neighborIdx != -1 {
						if neighborIdx == selfIdx {
							neighborIdx = s.particleListNextIdx[neighborIdx]
							continue
						}

						p1 := s.particles[neighborIdx]

						diffX := p1.posX - p0.posX

						if diffX > kernelRadius || diffX < -kernelRadius {
							neighborIdx = s.particleListNextIdx[neighborIdx]
							continue
						}

						diffY := p1.posY - p0.posY

						if diffY > kernelRadius || diffY < -kernelRadius {
							neighborIdx = s.particleListNextIdx[neighborIdx]
							continue
						}

						rSq := diffX*diffX + diffY*diffY

						if rSq < kernelRadiusSq {
							r := math.Sqrt(rSq)
							q := r * kernelRadiusInv
							closeness := 1 - q

							// inward radial velocity
							dx := diffX / r
							dy := diffY / r
							inwardVel := ((p0.velX-p1.velX)*dx + (p0.velY-p1.velY)*dy)

							if inwardVel > 1 {
								inwardVel = 1
							}

							if inwardVel > 0 {
								// linear and quadratic impulses
								I := closeness * (linViscosity*inwardVel + quadViscosity*inwardVel*inwardVel) * .5
								IX := I * dx
								IY := I * dy
								s.particles[selfIdx].velX -= IX
								s.particles[selfIdx].velY -= IY
								s.particles[neighborIdx].velX += IX
								s.particles[neighborIdx].velY += IY
							}
						}

						neighborIdx = s.particleListNextIdx[neighborIdx]
					}
				}
			}

			selfIdx = s.particleListNextIdx[selfIdx]
		}
	}
}

func (s *Simulator) adjustSprings(dt number) {
}

func (s *Simulator) applySpringDisplacements(dt number) {
}

func (s *Simulator) doubleDensityRelaxation(dt number) {
}

func (s *Simulator) resolveCollisions(dt number) {
}

func (s *Simulator) Update() {
	// TODO(cfoust): 07/07/24
	//this.screenMoveSmootherX += window.screenX - this.screenX;
	//this.screenMoveSmootherY += window.screenY - this.screenY;
	//this.screenX = window.screenX;
	//this.screenY = window.screenY;
	//const maxScreenMove = 50;
	//const screenMoveX = this.screenMoveSmootherX > maxScreenMove ? maxScreenMove : this.screenMoveSmootherX < -maxScreenMove ? -maxScreenMove : this.screenMoveSmootherX;
	//const screenMoveY = this.screenMoveSmootherY > maxScreenMove ? maxScreenMove : this.screenMoveSmootherY < -maxScreenMove ? -maxScreenMove : this.screenMoveSmootherY;

	//this.screenMoveSmootherX -= screenMoveX;
	//this.screenMoveSmootherY -= screenMoveY;

	// TODO(cfoust): 07/07/24
	//const dragX = this.mouseX - this.mousePrevX;
	//const dragY = this.mouseY - this.mousePrevY;
	//this.mousePrevX = this.mouseX;
	//this.mousePrevY = this.mouseY;

	s.populateHashGrid()

	dt := s.material.dt

	gravX := 0.02 * s.material.kernelRadius * s.material.gravX * dt
	gravY := 0.02 * s.material.kernelRadius * s.material.gravY * dt

	var attractRepel number
	if s.attract {
		attractRepel = 0.01 * s.material.kernelRadius
		attractRepel -= 0.01 * s.material.kernelRadius
	}

	arNonZero := attractRepel != 0

	for i := 0; i < len(s.particles); i++ {
		// apply gravity
		s.particles[i].velX += gravX
		s.particles[i].velY += gravY

		if arNonZero {
			dx := s.particles[i].posX - s.mouseX
			dy := s.particles[i].posY - s.mouseY
			distSq := dx*dx + dy*dy

			if distSq < 100000 && distSq > 0.1 {
				dist := math.Sqrt(distSq)
				invDist := 1 / dist

				dx *= invDist
				dy *= invDist

				s.particles[i].velX -= attractRepel * dx
				s.particles[i].velY -= attractRepel * dy
			}
		}

		if s.drag {
			dx := s.particles[i].posX - s.mouseX
			dy := s.particles[i].posY - s.mouseY
			distSq := dx*dx + dy*dy

			if distSq < 10000 && distSq > 0.1 {
				//dist := math.Sqrt(distSq)

				// TODO(cfoust): 07/07/24
				//s.particles[i].velX = dragX
				//s.particles[i].velY = dragY
			}
		}

		// TODO(cfoust): 07/07/24
		//s.particles[i].posX -= screenMoveX
		//s.particles[i].posY -= screenMoveY
	}

	s.applyViscosity(dt)

	for i := 0; i < len(s.particles); i++ {
		// save previous position
		s.particles[i].prevX = s.particles[i].posX
		s.particles[i].prevY = s.particles[i].posY

		// advance to predicted position
		s.particles[i].posX += s.particles[i].velX * dt
		s.particles[i].posY += s.particles[i].velY * dt
	}

	s.adjustSprings(dt)
	s.applySpringDisplacements(dt)
	s.doubleDensityRelaxation(dt)
	s.resolveCollisions(dt)

	dtInv := 1 / dt

	for i := 0; i < len(s.particles); i++ {
		// use previous position to calculate new velocity
		s.particles[i].velX = (s.particles[i].posX - s.particles[i].prevX) * dtInv
		s.particles[i].velY = (s.particles[i].posY - s.particles[i].prevY) * dtInv
	}
}
