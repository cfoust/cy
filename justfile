build:
  go build -o ./cy ./cmd/cy/...

install:
  go install ./cmd/cy/...

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
  go run ./cmd/cy/... -L dev

docs:
  cd docs && mdbook serve

api:
  go run ./cmd/docs/main.go

sqlc:
  cd pkg/db/ && sqlc generate
