build:
  go build -o ./cy ./cmd/cy/...

install:
  go install ./cmd/cy/...

test:
  go test ./pkg/... ./cmd/...

lint:
  go fmt ./pkg/... ./cmd/...

run:
  go run ./cmd/cy/... -L dev
