package cy

import (
	"os"
	"path/filepath"
	"runtime"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestAPI(t *testing.T) {
	_, apiTestFile, _, _ := runtime.Caller(0)
	tests, err := filepath.Glob(filepath.Join(
		filepath.Dir(apiTestFile),
		"api",
		"*_test.janet",
	))
	require.NoError(t, err)

	var failures []TestFailure
	for _, testFile := range tests {
		result, err := RunTestFile(testFile)
		require.NoError(t, err)
		failures = append(failures, result...)
	}

	if len(failures) == 0 {
		return
	}

	for _, failure := range failures {
		t.Logf(
			"%s: '%s' failed: %+v",
			failure.File,
			failure.Name,
			failure.Error,
		)
	}

	t.Errorf("%d API test(s) failed", len(failures))
}

// Test importing another Janet file from a config file. The API
// should still work.
func TestImport(t *testing.T) {
	server, _, err := NewTestServer()
	require.NoError(t, err)
	defer server.Cancel()

	d := t.TempDir()
	mainFile := filepath.Join(d, "main.janet")

	{
		f, err := os.Create(mainFile)
		require.NoError(t, err)
		_, _ = f.WriteString(`(import "./imported")`)
	}

	imported := filepath.Join(d, "imported.janet")
	{
		f, err := os.Create(imported)
		require.NoError(t, err)
		_, _ = f.WriteString(`
		(param/set :root :test 1)
		(key/action action/test "" (pp "hello"))
		`)
	}

	err = server.ExecuteFile(server.Ctx(), mainFile)
	require.NoError(t, err)

	err = server.Execute(server.Ctx(), `
(assert (= 1 (param/get :test :target :root)))
(assert (get (key/get-actions) "action/test"))
`)
	require.NoError(t, err)
}
