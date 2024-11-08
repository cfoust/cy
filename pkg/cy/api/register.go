package api

type RegisterModule struct {
	Registers Registers
}

func (c *RegisterModule) Set(
	register string,
	text string,
) error {
	return c.Registers.Set(register, text)
}

func (c *RegisterModule) Get(
	register string,
) (string, error) {
	return c.Registers.Get(register)
}
