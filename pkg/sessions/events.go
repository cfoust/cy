package sessions

import (
	"time"

	P "github.com/cfoust/cy/pkg/io/protocol"
)

type Event struct {
	Stamp   time.Time
	Message P.Message
}
