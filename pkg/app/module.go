package app

import (
	"io"

	"github.com/cfoust/cy/pkg/geom"
)

type App interface {
	io.ReadWriter
	Resize(size geom.Size) error
}
