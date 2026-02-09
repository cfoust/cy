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
		bucketX := int(math.Floor(s.particles[i].X * bucketSizeInv))
		bucketY := int(math.Floor(s.particles[i].Y * bucketSizeInv))
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

func (s *Simulator) applySpringDisplacements(dt number) {
	if s.material.springStiffness == 0 {
		return
	}

	kernelRadius := s.material.kernelRadius // h
	kernelRadiusInv := 1.0 / kernelRadius

	springStiffness := s.material.springStiffness * dt * dt
	plasticity := s.material.plasticity * dt // alpha
	yieldRatio := s.material.yieldRatio      // gamma
	minDistRatio := s.material.minDistRatio
	minDist := minDistRatio * kernelRadius

	for i := 0; i < len(s.particles); i++ {
		// TODO: maybe optimize this by using a list of springs instead of a hash
		for springIdx := 0; springIdx < len(s.particles[i].springs); springIdx++ {
			restLength := s.particles[i].springs[springIdx]

			springParticle := s.particles[springIdx]

			dx := s.particles[i].X - springParticle.X
			dy := s.particles[i].Y - springParticle.Y
			dist := math.Sqrt(dx*dx + dy*dy)

			tolerableDeformation := yieldRatio * restLength

			if dist > restLength+tolerableDeformation {
				restLength = restLength + plasticity*(dist-restLength-tolerableDeformation)
				s.particles[i].springs[springIdx] = restLength
			} else if dist < restLength-tolerableDeformation && dist > minDist {
				restLength = restLength - plasticity*(restLength-tolerableDeformation-dist)
				s.particles[i].springs[springIdx] = restLength
			}

			if restLength < minDist {
				restLength = minDist
				s.particles[i].springs[springIdx] = restLength
			}

			if restLength > kernelRadius {
				delete(s.particles[i].springs, springIdx)
				continue
			}

			D := springStiffness * (1 - restLength*kernelRadiusInv) * (dist - restLength) / dist
			dx *= D
			dy *= D

			s.particles[i].X -= dx
			s.particles[i].Y -= dy

			s.particles[springIdx].X += dx
			s.particles[springIdx].Y += dy
		}
	}
}

func (s *Simulator) doubleDensityRelaxation(dt number) {
	kernelRadius := s.material.kernelRadius // h
	kernelRadiusSq := kernelRadius * kernelRadius
	kernelRadiusInv := 1.0 / kernelRadius

	restDensity := s.material.restDensity
	stiffness := s.material.stiffness * dt * dt
	nearStiffness := s.material.nearStiffness * dt * dt

	minDistRatio := s.material.minDistRatio
	minDist := minDistRatio * kernelRadius

	// TODO(cfoust): 07/07/24 this is very wasteful
	// Neighbor cache
	neighbors := make([]int, len(s.particles))
	neighborUnitX := make([]number, len(s.particles))
	neighborUnitY := make([]number, len(s.particles))
	neighborCloseness := make([]number, len(s.particles))
	visitedBuckets := make([]int, s.numHashBuckets)

	numActiveBuckets := s.numActiveBuckets

	addSprings := s.material.springStiffness > 0

	for abIdx := 0; abIdx < numActiveBuckets; abIdx++ {
		selfIdx := s.particleListHeads[s.activeBuckets[abIdx]]

		for selfIdx != -1 {
			p0 := s.particles[selfIdx]

			density := 0.
			nearDensity := 0.

			numNeighbors := 0
			numVisitedBuckets := 0

			// Compute density and near-density
			bucketX := math.Floor(p0.X * kernelRadiusInv)
			bucketY := math.Floor(p0.Y * kernelRadiusInv)

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

						diffX := p1.X - p0.X

						if diffX > kernelRadius || diffX < -kernelRadius {
							neighborIdx = s.particleListNextIdx[neighborIdx]
							continue
						}

						diffY := p1.Y - p0.Y

						if diffY > kernelRadius || diffY < -kernelRadius {
							neighborIdx = s.particleListNextIdx[neighborIdx]
							continue
						}

						rSq := diffX*diffX + diffY*diffY

						if rSq < kernelRadiusSq {
							r := math.Sqrt(rSq)
							q := r * kernelRadiusInv
							closeness := 1 - q
							closenessSq := closeness * closeness

							density += closeness * closeness
							nearDensity += closeness * closenessSq

							neighbors[numNeighbors] = neighborIdx
							neighborUnitX[numNeighbors] = diffX / r
							neighborUnitY[numNeighbors] = diffY / r
							neighborCloseness[numNeighbors] = closeness
							numNeighbors++

							// Add spring if not already present
							// TODO: this JS hash thing is absolutely crazy but curious how it performs
							if addSprings && selfIdx < neighborIdx &&
								r > minDist &&
								p0.springs[neighborIdx] == 0. {
								s.particles[selfIdx].springs[neighborIdx] = r
							}
						}

						neighborIdx = s.particleListNextIdx[neighborIdx]
					}
				}
			}

			// Compute pressure and near-pressure
			pressure := stiffness * (density - restDensity)
			nearPressure := nearStiffness * nearDensity

			// Optional: Clamp pressure for stability
			// const pressureSum = pressure + nearPressure;

			// if (pressureSum > 1) {
			//   const pressureMul = 1 / pressureSum;
			//   pressure *= pressureMul;
			//   nearPressure *= pressureMul;
			// }

			if pressure > 1 {
				pressure = 1
			}

			if nearPressure > 1 {
				nearPressure = 1
			}

			dispX := 0.
			dispY := 0.

			for j := 0; j < numNeighbors; j++ {
				p1 := neighbors[j]

				closeness := neighborCloseness[j]
				D := (pressure*closeness + nearPressure*closeness*closeness) / 2
				DX := D * neighborUnitX[j]
				DY := D * neighborUnitY[j]

				s.particles[p1].X += DX
				s.particles[p1].Y += DY

				dispX -= DX
				dispY -= DY

				// p0.posX -= DX;
				// p0.posY -= DY;
			}

			s.particles[selfIdx].X += dispX
			s.particles[selfIdx].Y += dispY

			selfIdx = s.particleListNextIdx[selfIdx]
		}
	}
}

const (
	FLUID_PADDING = 1.5
)

func (s *Simulator) resolveCollisions(dt number) {
	boundaryMul := 0.5 * dt * dt
	boundaryMinX := FLUID_PADDING
	boundaryMaxX := s.width - FLUID_PADDING
	boundaryMinY := FLUID_PADDING
	boundaryMaxY := s.height - FLUID_PADDING

	for i, p := range s.particles {
		if p.X < boundaryMinX {
			s.particles[i].X += boundaryMul * (boundaryMinX - p.X)
		} else if p.X > boundaryMaxX {
			s.particles[i].X += boundaryMul * (boundaryMaxX - p.X)
		}

		inBottomHole := s.drainEnabled &&
			p.X >= s.bottomHoleMinX &&
			p.X <= s.bottomHoleMaxX

		if inBottomHole && p.Y > boundaryMaxY {
			// Map X proportionally from bottom hole to top hole
			t := (p.X - s.bottomHoleMinX) /
				(s.bottomHoleMaxX - s.bottomHoleMinX)
			s.particles[i].X = s.topHoleMinX +
				t*(s.topHoleMaxX-s.topHoleMinX)
			s.particles[i].prevX = s.particles[i].X
			// Wrap to top
			s.particles[i].Y = boundaryMinY + FLUID_PADDING
			s.particles[i].prevY = s.particles[i].Y
		} else {
			if p.Y < boundaryMinY {
				s.particles[i].Y += boundaryMul * (boundaryMinY - p.Y)
			} else if p.Y > boundaryMaxY {
				s.particles[i].Y += boundaryMul * (boundaryMaxY - p.Y)
			}
		}
	}
}

func (s *Simulator) Update(dt float64) {
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
			dx := s.particles[i].X - s.mouseX
			dy := s.particles[i].Y - s.mouseY
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

		// TODO(cfoust): 07/07/24 - drag interaction disabled
		// if s.drag {
		//	dx := s.particles[i].X - s.mouseX
		//	dy := s.particles[i].Y - s.mouseY
		//	distSq := dx*dx + dy*dy
		//	if distSq < 10000 && distSq > 0.1 {
		//		dist := math.Sqrt(distSq)
		//		s.particles[i].velX = dragX
		//		s.particles[i].velY = dragY
		//	}
		// }

		// TODO(cfoust): 07/07/24
		//s.particles[i].posX -= screenMoveX
		//s.particles[i].posY -= screenMoveY
	}

	//s.applyViscosity(dt)

	for i := 0; i < len(s.particles); i++ {
		// save previous position
		s.particles[i].prevX = s.particles[i].X
		s.particles[i].prevY = s.particles[i].Y

		// advance to predicted position
		s.particles[i].X += s.particles[i].velX * dt
		s.particles[i].Y += s.particles[i].velY * dt
	}

	s.applySpringDisplacements(dt)
	s.doubleDensityRelaxation(dt)
	s.resolveCollisions(dt)

	dtInv := 1 / dt

	for i := 0; i < len(s.particles); i++ {
		// use previous position to calculate new velocity
		s.particles[i].velX = (s.particles[i].X - s.particles[i].prevX) * dtInv
		s.particles[i].velY = (s.particles[i].Y - s.particles[i].prevY) * dtInv
	}
}
