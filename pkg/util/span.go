package util

import (
	"time"

	"github.com/rs/zerolog/log"
)

func Span(name string) func() {
	start := time.Now()
	return func() {
		log.Info().Msgf("span: %s %+v", name, time.Since(start))
	}
}
