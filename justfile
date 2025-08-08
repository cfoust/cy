build:
  go build -o ./cy ./cmd/cy/...

install:
  go install ./cmd/cy/...

test:
  go test ./pkg/... ./cmd/...

stories:
  go run ./cmd/stories/...

format:
  go fmt ./pkg/... ./cmd/...

generate:
  go generate ./pkg/... ./cmd/...

run:
  go run ./cmd/cy/... -L dev

docs:
  cd docs && mdbook serve

api:
  go run ./cmd/docs/main.go
