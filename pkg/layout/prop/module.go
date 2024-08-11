package prop

import (
	"context"
	"fmt"
	"time"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/style"

	"github.com/rs/zerolog"
)

type Prop[T any] struct {
	isStatic bool
	static   T
	dynamic  *janet.Function

	havePreset bool
	ctx        context.Context
	context    interface{}
	params     []interface{}

	log zerolog.Logger
}

func (p *Prop[T]) Static() (value T, ok bool) {
	if !p.isStatic {
		return
	}

	return p.static, true
}

func (p *Prop[T]) SetLogger(log zerolog.Logger) {
	if p == nil {
		return
	}

	// We want to be careful not to spam the user
	sampler := &zerolog.BurstSampler{
		Burst:  5,
		Period: 1 * time.Second,
	}
	sampler.NextSampler = sampler
	p.log = log.Sample(sampler)
}

type Presettable interface {
	Preset(
		ctx context.Context,
		context interface{},
		params ...interface{},
	)
	SetLogger(log zerolog.Logger)
}

func (p *Prop[T]) Preset(
	ctx context.Context,
	context interface{},
	params ...interface{},
) {
	if p == nil {
		return
	}

	p.havePreset = true
	p.ctx = ctx
	p.context = context
	p.params = params
}

func (p *Prop[T]) Get(
	ctx context.Context,
	context interface{},
	params ...interface{},
) (value T, ok bool) {
	// This is actually OK--we don't want the caller to have to worry
	// about checking != nil all the time
	if p == nil {
		return
	}

	if p.isStatic {
		return p.static, true
	}

	janetValue, err := p.dynamic.CallResult(
		ctx,
		context,
		params...,
	)
	if err != nil {
		p.log.Error().Msgf(
			"error calculating property: %s",
			err,
		)
		return
	}

	err = janetValue.Unmarshal(&value)
	if err != nil {
		p.log.Error().Msgf(
			"error unmarshaling result of dynamic property: %s",
			err,
		)
		return
	}
	ok = err == nil
	return
}

func (p *Prop[T]) GetPreset() (value T, ok bool) {
	if p == nil {
		return
	}

	if p.isStatic {
		return p.static, true
	}

	if !p.havePreset {
		p.log.Error().Msgf("prop is missing presets")
		return value, false
	}

	return p.Get(
		p.ctx,
		p.context,
		p.params...,
	)
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
	if p == nil {
		return nil
	}

	if p.isStatic {
		return p.static
	}

	return p.dynamic
}

type String = Prop[string]
type Color = Prop[*style.Color]
type Border = Prop[*style.Border]
