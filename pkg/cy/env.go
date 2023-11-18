package cy

type Environment map[string]string

func (e Environment) Get(key string) (string, bool) {
	value, ok := e[key]
	return value, ok
}

func (e Environment) Default(key string, defaultValue string) string {
	value, ok := e[key]
	if !ok {
		value = defaultValue
	}

	return value
}

func (e Environment) IsSet(key string) bool {
	value, ok := e[key]
	if !ok {
		return false
	}
	return len(value) > 0
}
