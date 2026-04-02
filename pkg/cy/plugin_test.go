package cy

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestLoadPlugins(t *testing.T) {
	pluginDir := t.TempDir()

	// Create two plugins in alphabetical order
	for _, name := range []string{"alpha", "beta"} {
		dir := filepath.Join(pluginDir, name)
		require.NoError(t, os.MkdirAll(dir, 0o755))
		require.NoError(t, os.WriteFile(
			filepath.Join(dir, "init.janet"),
			[]byte(`(defn `+name+`/hello [] "`+name+`")`),
			0o644,
		))
	}

	// Create a subdirectory without init.janet (should be skipped)
	require.NoError(t, os.MkdirAll(
		filepath.Join(pluginDir, "no-init"),
		0o755,
	))

	// Create a regular file (not a directory, should be skipped)
	require.NoError(t, os.WriteFile(
		filepath.Join(pluginDir, "not-a-dir.txt"),
		[]byte("hello"),
		0o644,
	))

	server, err := Start(context.Background(), Options{
		Shell:     "/bin/bash",
		SkipInput: true,
		PluginDir: pluginDir,
	})
	require.NoError(t, err)
	defer server.Cancel()

	// Both plugins should have been loaded
	err = server.Execute(server.Ctx(), `
(assert (= "alpha" (alpha/hello)))
(assert (= "beta" (beta/hello)))
`)
	require.NoError(t, err)
}

func TestLoadPluginsEmpty(t *testing.T) {
	// Empty PluginDir — should not error
	server, err := Start(context.Background(), Options{
		Shell:     "/bin/bash",
		SkipInput: true,
	})
	require.NoError(t, err)
	defer server.Cancel()
}

func TestLoadPluginError(t *testing.T) {
	pluginDir := t.TempDir()

	// Create a plugin with a syntax error
	badDir := filepath.Join(pluginDir, "bad-plugin")
	require.NoError(t, os.MkdirAll(badDir, 0o755))
	require.NoError(t, os.WriteFile(
		filepath.Join(badDir, "init.janet"),
		[]byte(`(this is not valid janet`),
		0o644,
	))

	// Create a valid plugin
	goodDir := filepath.Join(pluginDir, "good-plugin")
	require.NoError(t, os.MkdirAll(goodDir, 0o755))
	require.NoError(t, os.WriteFile(
		filepath.Join(goodDir, "init.janet"),
		[]byte(`(defn good-plugin/hello [] "works")`),
		0o644,
	))

	// Server should still start — bad plugin is skipped
	server, err := Start(context.Background(), Options{
		Shell:     "/bin/bash",
		SkipInput: true,
		PluginDir: pluginDir,
	})
	require.NoError(t, err)
	defer server.Cancel()

	// Good plugin should still have loaded
	err = server.Execute(server.Ctx(), `
(assert (= "works" (good-plugin/hello)))
`)
	require.NoError(t, err)
}
