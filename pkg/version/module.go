package version

// These should be set via go build -ldflags -X 'xxxx'.
var (
	Version   = "development"
	GoVersion = "1.21"
	GitCommit = "unknown"
	BuildTime = "1979-01-09T16:09:53+00:00"
)
