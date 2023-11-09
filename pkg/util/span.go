package util

import (
	"github.com/rs/zerolog/log"
	"time"
)

func Span(name string) func() {
	start := time.Now()
	return func() {
		log.Info().Msgf("span: %s %+v", name, time.Now().Sub(start))
	}
}
