package api

type ExecModule struct {
	Server Server
}

func (e *ExecModule) File(path string) error {
	return e.Server.ExecuteJanet(path)
}
