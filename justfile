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
  go tool golines -m 80 -w ./pkg/ ./cmd/
  go tool gci write --custom-order -s standard -s "prefix(github.com/cfoust/cy)" -s default ./pkg/ ./cmd/
  go run ./cmd/janet-format/... $(find pkg cmd -name '*.janet')

lint:
  go tool revive -config revive.toml -exclude pkg/janet/test/module.go -set_exit_status ./pkg/... ./cmd/...

generate:
  go generate ./pkg/... ./cmd/...

run:
  go run -ldflags "{{ldflags}}" ./cmd/cy/... -L dev

docs:
  cd docs && npm start

docs-build:
  rm -rf docs/.docusaurus
  cd docs && npm run build

api:
  go run ./cmd/docs/main.go

sqlc:
  cd pkg/db/ && sqlc generate

ci:
  just generate
  just format
  go mod tidy
  just lint
  just build
  just test-race
