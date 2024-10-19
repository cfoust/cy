package slime

import (
	"math"
	"math/rand"

	"github.com/cfoust/cy/pkg/geom/image"
)

const (
	WIDTH      int = 400
	HEIGHT     int = 400
	NUM_AGENTS     = 1
	DECAY          = 0.9
	MIN_CHEM       = 0.0001
	ASPECT         = 0.5017144097222223

	SENS_ANGLE = 45 * math.Pi / 180
	SENS_DIST  = 9
	AGT_SPEED  = 1
	AGT_ANGLE  = 45 * math.Pi / 180
	DEPOSIT    = 1

	OOB     = ' '
	TEXTURE = "  ``^@..„v0"
)

type Vec2 struct {
	X, Y float64
}

func (v Vec2) Add(other Vec2) Vec2 {
	return Vec2{v.X + other.X, v.Y + other.Y}
}

func (v Vec2) MulN(n float64) Vec2 {
	return Vec2{v.X * n, v.Y * n}
}

func (v Vec2) Rot(angle float64) Vec2 {
	cosA := math.Cos(angle)
	sinA := math.Sin(angle)
	return Vec2{v.X*cosA - v.Y*sinA, v.X*sinA + v.Y*cosA}
}

type Agent struct {
	Pos     Vec2
	Dir     Vec2
	scatter bool
}

func (a *Agent) Sense(m float64, chem []float32) float64 {
	senseVec := a.Dir.Rot(m * SENS_ANGLE).MulN(SENS_DIST)
	pos := Vec2{math.Floor(a.Pos.X + senseVec.X), math.Floor(a.Pos.Y + senseVec.Y)}
	if !bounded(pos) {
		return -1
	}
	sensed := chem[int(pos.Y)*HEIGHT+int(pos.X)]
	if a.scatter {
		return 1 - float64(sensed)
	}
	return float64(sensed)
}

func (a *Agent) React(chem []float32) {
	forwardChem := a.Sense(0, chem)
	leftChem := a.Sense(-1, chem)
	rightChem := a.Sense(1, chem)

	rotate := 0.0
	if forwardChem > leftChem && forwardChem > rightChem {
		rotate = 0
	} else if forwardChem < leftChem && forwardChem < rightChem {
		if rand.Float64() < 0.5 {
			rotate = -AGT_ANGLE
		} else {
			rotate = AGT_ANGLE
		}
	} else if leftChem < rightChem {
		rotate = AGT_ANGLE
	} else if rightChem < leftChem {
		rotate = -AGT_ANGLE
	} else if forwardChem < 0 {
		rotate = math.Pi / 2
	}

	a.Dir = a.Dir.Rot(rotate)
	a.Pos = a.Pos.Add(a.Dir.MulN(AGT_SPEED))
}

func (a *Agent) Deposit(chem []float32) {
	x := int(math.Floor(a.Pos.X))
	y := int(math.Floor(a.Pos.Y))
	idx := y*HEIGHT + x
	if idx < 0 || idx >= len(chem) {
		return
	}

	chem[idx] = float32(math.Min(1, float64(chem[idx])+DEPOSIT))
}

func bounded(vec Vec2) bool {
	r := math.Min(float64(WIDTH), float64(HEIGHT)) / 2
	return ((vec.X-r)*(vec.X-r)+(vec.Y-r)*(vec.Y-r) <= r*r)
}

func blur(row, col int, data []float32) float32 {
	sum := float32(0)
	for dy := -1; dy <= 1; dy++ {
		for dx := -1; dx <= 1; dx++ {
			idx := (row+dy)*HEIGHT + col + dx
			if idx >= 0 && idx < len(data) {
				sum += data[idx]
			}
		}
	}
	return sum / 9
}

func randCircle() Vec2 {
	r := math.Sqrt(rand.Float64())
	theta := rand.Float64() * 2 * math.Pi
	return Vec2{r * math.Cos(theta), r * math.Sin(theta)}
}

type Cursor struct {
	pressed bool
	x, y    float64
}

type Simulator struct {
	frame     int
	rows      int
	cols      int
	chem      []float32
	wip       []float32
	agents    []Agent
	viewScale Vec2
	viewFocus Vec2
}

func (s *Simulator) updateView(cursor Cursor) {
	var targetScale Vec2
	if cursor.pressed {
		// Zoom in for detailed view
		targetScale = Vec2{Y: 1 / ASPECT, X: 1}
	} else if float64(s.rows)/ASPECT < float64(s.cols) {
		// Fit to wide window
		targetScale = Vec2{
			Y: 1.1 * float64(WIDTH) / float64(s.rows),
			X: 1.1 * float64(WIDTH) / float64(s.rows) * ASPECT,
		}
	} else {
		// Fit to tall window
		targetScale = Vec2{
			Y: 1.1 * float64(WIDTH) / float64(s.cols) / ASPECT,
			X: 1.1 * float64(WIDTH) / float64(s.cols),
		}
	}

	// Smooth transition to new scale
	s.viewScale.Y += 0.1 * (targetScale.Y - s.viewScale.Y)
	s.viewScale.X += 0.1 * (targetScale.X - s.viewScale.X)

	var targetFocus Vec2
	if cursor.pressed {
		targetFocus = Vec2{Y: cursor.y / float64(s.rows), X: cursor.x / float64(s.cols)}
	} else {
		targetFocus = Vec2{Y: 0.5, X: 0.5}
	}

	// Smooth transition to new focus
	s.viewFocus.Y += 0.1 * (targetFocus.Y - s.viewFocus.Y)
	s.viewFocus.X += 0.1 * (targetFocus.X - s.viewFocus.X)
}

func (s *Simulator) Step(cursor Cursor) {
	// Diffuse & decay
	for row := 0; row < HEIGHT; row++ {
		for col := 0; col < WIDTH; col++ {
			val := DECAY * blur(row, col, s.chem)
			if val < MIN_CHEM {
				val = 0
			}
			s.wip[row*HEIGHT+col] = val
		}
	}

	// Swap buffers
	s.chem, s.wip = s.wip, s.chem

	// Sense, rotate, and move agents
	isScattering := math.Sin(float64(s.frame)/150) > 0.8
	for i := range s.agents {
		s.agents[i].scatter = isScattering
		s.agents[i].React(s.chem)
	}

	// Deposit by agents
	for i := range s.agents {
		s.agents[i].Deposit(s.chem)
	}

	// Update view parameters
	s.updateView(cursor)

	s.frame++
}

func (s *Simulator) renderCell(row, col int) rune {
	offset := Vec2{
		Y: math.Floor(s.viewFocus.Y * (float64(HEIGHT) - s.viewScale.Y*float64(s.rows))),
		X: math.Floor(s.viewFocus.X * (float64(WIDTH) - s.viewScale.X*float64(s.cols))),
	}

	x := float64(col)
	y := float64(row)

	sampleFrom := Vec2{
		Y: offset.Y + math.Floor(y*s.viewScale.Y),
		X: offset.X + math.Floor(x*s.viewScale.X),
	}

	sampleTo := Vec2{
		Y: offset.Y + math.Floor((y+1)*s.viewScale.Y),
		X: offset.X + math.Floor((x+1)*s.viewScale.X),
	}

	if !bounded(sampleFrom) || !bounded(sampleTo) {
		return OOB
	}

	sampleH := math.Max(1, sampleTo.Y-sampleFrom.Y)
	sampleW := math.Max(1, sampleTo.X-sampleFrom.X)

	maxVal, sumVal := 0.0, 0.0
	for x := sampleFrom.X; x < sampleFrom.X+sampleW; x++ {
		for y := sampleFrom.Y; y < sampleFrom.Y+sampleH; y++ {
			val := s.chem[int(y)*HEIGHT+int(x)]
			maxVal = math.Max(maxVal, float64(val))
			sumVal += float64(val)
		}
	}

	val := sumVal / (sampleW * sampleH)
	val = (val + maxVal) / 2
	val = math.Pow(val, 1.0/3.0)

	// Convert to texture character
	texRow := (col + row) % len(TEXTURE)
	texCol := int(math.Ceil(val * float64(len(TEXTURE)-1)))
	idx := texRow*len(TEXTURE) + texCol
	if idx < 0 || idx >= len(TEXTURE) {
		return OOB
	}

	//char := TEXTURE[texRow*len(TEXTURE)+texCol]
	//return rune(char)
	return 'a'
}

func (s *Simulator) Render(out image.Image) {
	size := out.Size()
	for row := 0; row < size.R; row++ {
		for col := 0; col < size.C; col++ {
			out[row][col].Char = s.renderCell(row, col)
		}
	}
}

func New(rows, cols int, agents []Agent) *Simulator {
	chem := make([]float32, HEIGHT*WIDTH)
	wip := make([]float32, HEIGHT*WIDTH)

	agents = make([]Agent, NUM_AGENTS)
	for i := 0; i < NUM_AGENTS; i++ {
		agents[i] = Agent{
			Pos: randCircle().
				MulN(0.5).
				Add(Vec2{1, 1}).
				MulN(0.5 * float64(WIDTH)),
			Dir: Vec2{1, 0}.Rot(rand.Float64() * 2 * math.Pi),
		}
	}

	s := &Simulator{
		rows:      rows,
		cols:      cols,
		chem:      chem,
		wip:       wip,
		agents:    agents,
		viewScale: Vec2{1, 1},
		viewFocus: Vec2{0.5, 0.5},
	}

	s.updateView(Cursor{})

	return s
}
