build:
  go build -o ./cy ./cmd/cy/...

install:
  go install ./cmd/cy/...

test:
  go test ./pkg/... ./cmd/...

format:
  go fmt ./pkg/... ./cmd/...

run:
  go run ./cmd/cy/... -L dev

serve-docs:
  cd docs && mdbook serve
