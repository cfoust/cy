package cy

import (
	"fmt"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"
)

// TestFailure represents a single failed test case.
type TestFailure struct {
	File  string
	Name  string
	Error error
}

// RunTestFile starts an ephemeral cy server, loads the test
// framework, and executes the given Janet test file. Returns any
// test failures.
func RunTestFile(file string) ([]TestFailure, error) {
	server, create, err := NewTestServer()
	if err != nil {
		return nil, err
	}
	defer server.Cancel()

	var failures []TestFailure
	_ = server.Callback("run-test", "", func(
		name string,
		context bool,
		callback *janet.Function,
	) {
		var callContext interface{}
		if context {
			client, err := create(geom.DEFAULT_SIZE)
			if err != nil {
				failures = append(failures, TestFailure{
					Name:  name,
					File:  file,
					Error: fmt.Errorf("failed to create client: %w", err),
				})
				return
			}
			callContext = client
			defer client.Cancel()
		}

		defer server.tree.Reset()

		err := callback.Call(
			server.Ctx(),
			callContext,
			janet.Params{},
		)
		if err == nil {
			return
		}

		failures = append(failures, TestFailure{
			Name:  name,
			File:  file,
			Error: err,
		})
	})

	_, err = server.ExecuteCall(
		server.Ctx(),
		nil,
		janet.Call{
			Code:       TEST_FILE,
			SourcePath: "test.janet",
			Options:    janet.DEFAULT_CALL_OPTIONS,
		},
	)
	if err != nil {
		return nil, fmt.Errorf(
			"failed to load test framework: %w",
			err,
		)
	}

	err = server.ExecuteFile(server.Ctx(), file)
	if err != nil {
		return nil, fmt.Errorf(
			"failed to execute %s: %w",
			file,
			err,
		)
	}

	return failures, nil
}
