build:
  go build -o ./cy ./cmd/cy/...

install:
  go install ./cmd/cy/...

test:
  go test ./pkg/... ./cmd/...

stories *args:
  go run ./cmd/stories/... {{args}}

format:
  go fmt ./pkg/... ./cmd/...

lint:
  golangci-lint run

lint-fix:
  golangci-lint run --fix

install-linters:
  go install github.com/segmentio/golines@v0.12.2
  go install github.com/daixiang0/gci@v0.13.7

generate:
  go generate ./pkg/... ./cmd/...

run:
  go run ./cmd/cy/... -L dev

docs:
  cd docs && mdbook serve

api:
  go run ./cmd/docs/main.go
