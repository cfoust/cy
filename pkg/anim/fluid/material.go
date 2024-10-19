package fluid

type Material struct {
	name          string // name;
	restDensity   number // restDensity;
	stiffness     number // stiffness;
	nearStiffness number // nearStiffness;
	kernelRadius  number // kernelRadius;
	pointSize     number // 5;
	gravX         number // 0.0;
	gravY         number // 0.5;
	dt            number // 1;

	springStiffness number // 0.0;
	plasticity      number // 0.5; // alpha
	yieldRatio      number // 0.25; // gamma
	minDistRatio    number // .25;
	linViscosity    number // 0.0;
	quadViscosity   number // 0.1;
	maxPressure     number // 1;
}

func NewMaterial(name string, restDensity, stiffness, nearStiffness, kernelRadius number) Material {
	return Material{
		name:            name,
		restDensity:     restDensity,
		stiffness:       stiffness,
		nearStiffness:   nearStiffness,
		kernelRadius:    kernelRadius,
		pointSize:       5,
		gravX:           0.0,
		gravY:           0.5,
		dt:              1,
		springStiffness: 0.0,
		plasticity:      0.5,  // alpha
		yieldRatio:      0.25, // gamma
		minDistRatio:    0.25, // prevents the springs from getting too short
		linViscosity:    0.0,
		quadViscosity:   0.1,
		maxPressure:     1,
	}
}
