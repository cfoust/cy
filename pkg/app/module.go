package app

import (
	"io"

	"github.com/cfoust/cy/pkg/geom"
)

type Size = geom.Size

type App interface {
	io.ReadWriter
	Resize(size Size) error
}
