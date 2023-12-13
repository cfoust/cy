package version

// These should be set via go build -ldflags -X 'xxxx'.
var (
	Version   = "development"
	GoVersion = "1.21"
	GitCommit = "unknown"
	BuildTime = "unknown"
)
