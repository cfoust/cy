package prop

import (
	"fmt"
	"time"

	"github.com/cfoust/cy/pkg/janet"
)

func (p *Prop[T]) UnmarshalJanet(value *janet.Value) error {
	var fun *janet.Function
	err := value.Unmarshal(&fun)
	if err == nil {
		p.dynamic = fun
		p.ttl = time.Second
		return nil
	}

	var staticValue T
	err = value.Unmarshal(&staticValue)
	if err == nil {
		p.isStatic = true
		p.static = staticValue
		return nil
	}

	return fmt.Errorf(
		"property must be function or static value: %s",
		err,
	)
}

func (p *Prop[T]) MarshalJanet() interface{} {
	if p == nil {
		return nil
	}

	if p.isStatic {
		return p.static
	}

	return p.dynamic
}
