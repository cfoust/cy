package cy

import (
	"path/filepath"
	"runtime"
	"testing"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"

	"github.com/stretchr/testify/require"
)

type testFailure struct {
	File  string
	Name  string
	Error error
}

func runTestFile(t *testing.T, file string) (failures []testFailure) {
	server, create := setup(t)

	server.Callback("run-test", "", func(
		name string,
		callback *janet.Function,
	) {
		client := create(geom.DEFAULT_SIZE)
		err := callback.CallContext(
			server.Ctx(),
			client,
		)
		if err == nil {
			return
		}
		failures = append(failures, testFailure{
			Name:  name,
			File:  file,
			Error: err,
		})
	})

	err := server.ExecuteFile(server.Ctx(), file)
	require.NoError(t, err)
	return
}

func TestAPI(t *testing.T) {
	_, apiTestFile, _, _ := runtime.Caller(0)
	tests, err := filepath.Glob(filepath.Join(
		filepath.Dir(apiTestFile),
		"api",
		"*_test.janet",
	))
	require.NoError(t, err)

	var failures []testFailure
	for _, testFile := range tests {
		failures = append(failures, runTestFile(t, testFile)...)
	}

	if len(failures) == 0 {
		return
	}

	for _, failure := range failures {
		t.Logf("%s:%s failed: %+v", failure.File, failure.Name, failure.Error)
	}

	t.Errorf("%d API test(s) failed", len(failures))
}
