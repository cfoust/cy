package api

import (
	"time"
)

// TimeModule provides functions for working with time.Time values.
// TODO(cfoust): 09/15/24 Should this go in pkg/janet instead?
type TimeModule struct{}

func (t *TimeModule) Now() time.Time {
	return time.Now()
}

func (t *TimeModule) Format(ts time.Time, layout string) string {
	return ts.Format(layout)
}
