package cy

import (
	"regexp"
)

var (
	CONTEXT_ENV   = "CY"
	CONTEXT_REGEX = regexp.MustCompile("^(?P<socket>\\w+):(?P<id>\\d+)$")
	// Regex used for validating socket names, which must be alphanumeric and not
	// contain spaces.
	SOCKET_REGEX = regexp.MustCompile("^(\\w+)$")
)
