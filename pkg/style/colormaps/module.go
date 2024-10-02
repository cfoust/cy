package colormaps

import (
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/style"
)

//go:generate go run gen.go

type Scheme struct {
	ID   janet.Keyword
	Name string
	Map  *style.ColorMap
}
