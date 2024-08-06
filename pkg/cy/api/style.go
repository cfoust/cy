package api

import (
	"github.com/cfoust/cy/pkg/style"
)

type StyleModule struct {
}

func (s *StyleModule) Render(style *style.Style, text string) string {
	return style.Render(text)
}
