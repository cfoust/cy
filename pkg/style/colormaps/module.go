package colormaps

import (
	"github.com/cfoust/cy/pkg/style"
)

//go:generate go run gen.go

type Scheme struct {
	ID   string
	Name string
	Map  *style.ColorMap
}
