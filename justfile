version_pkg := "github.com/cfoust/cy/pkg/version"
version := `git describe --tags --always --dirty 2>/dev/null || echo "development"`
go_version := `go version | awk '{print $3}'`
git_commit := `git rev-parse --short HEAD 2>/dev/null || echo "unknown"`
build_time := `date -u '+%Y-%m-%dT%H:%M:%SZ'`
ldflags := "-X " + version_pkg + ".Version=" + version + " -X " + version_pkg + ".GoVersion=" + go_version + " -X " + version_pkg + ".GitCommit=" + git_commit + " -X " + version_pkg + ".BuildTime=" + build_time

build:
  go build -ldflags "{{ldflags}}" -o ./cy ./cmd/cy/...

install:
  go install -ldflags "{{ldflags}}" ./cmd/cy/...

test *args:
  go test ./pkg/... ./cmd/... {{args}}

test-race *args:
  go test -race ./pkg/... ./cmd/... {{args}}

stories *args:
  go run ./cmd/stories/... {{args}}

format:
  go fmt ./pkg/... ./cmd/...

lint:
  go tool golangci-lint run

lint-fix:
  go tool golangci-lint run --fix

generate:
  go generate ./pkg/... ./cmd/...

run:
  go run -ldflags "{{ldflags}}" ./cmd/cy/... -L dev

docs:
  cd docs && mdbook serve

api:
  go run ./cmd/docs/main.go

sqlc:
  cd pkg/db/ && sqlc generate
