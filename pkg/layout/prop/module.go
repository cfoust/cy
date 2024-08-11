package prop

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/style"
)

type Prop[T any] struct {
	isStatic bool
	static   T
	dynamic  *janet.Function
	context  interface{}
}

func (p *Prop[T]) Get(
	ctx context.Context,
	user interface{},
	params ...interface{},
) (T, error) {
	if p.isStatic {
		return p.static, nil
	}

	janetValue, _ := p.dynamic.CallResult(
		ctx,
		p.context,
		params...,
	)

	var value T
	err := janetValue.Unmarshal(&value)
	return value, err
}

func (p *Prop[T]) UnmarshalJanet(value *janet.Value) error {
	var fun *janet.Function
	err := value.Unmarshal(&fun)
	if err == nil {
		p.dynamic = fun
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
	if p.isStatic {
		return p.static
	}

	return p.dynamic
}

type String = Prop[string]
type Color = Prop[*style.Color]
