package janet

import (
	"regexp"
	"strings"
)

var (
	METHOD_REGEX = regexp.MustCompile("^# doc: (\\w+)$")
)

// Documented provides documentation in the form of a Markdown-formatted
// string. All of the lines following a top-level Markdown header with a title
// that matches a Go method name will be included as the Janet documentation
// for that method.
//
// For example, providing a string that looks like this:
// ```
// # doc: SomeMethod
// This is some documentation
// # doc: SomeMethodB
// This is some other documentation
// ```
// Will result in SomeMethod and SomeMethodB having those docstrings.
type Documented interface {
	Documentation() string
}

func parseDocstrings(markdown string) (result map[string]string) {
	result = make(map[string]string)

	name := ""
	docstring := ""
	for _, line := range strings.Split(markdown, "\n") {
		match := METHOD_REGEX.FindStringSubmatch(line)
		if match == nil {
			docstring += line + "\n"
			continue
		}

		if len(name) > 0 {
			result[name] = docstring
		}
		name = match[1]
		docstring = ""
	}

	if len(name) > 0 {
		result[name] = docstring
	}

	return
}
