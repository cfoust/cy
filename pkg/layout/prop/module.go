package prop

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/style"

	"github.com/rs/zerolog"
)

// Prop is a property that can be either static or dynamic. If it is static, it
// will always return the same value. If it is dynamic, it will call a Janet
// function to calculate the value.
type Prop[T any] struct {
	isStatic bool

	// A static value that will be returned if the property is static.
	static T
	// The Janet function that will be called to calculate the property.
	dynamic *janet.Function

	// Whether the user provided preset arguments.
	havePreset bool

	// All presets that will be used to calculate the property.
	ctx     context.Context
	context interface{}
	params  []interface{}

	cacheExpires time.Time
	cachedValue  T
	ttl          time.Duration

	log zerolog.Logger
}

// Static reports whether the property is static. If it is, it will return the
// value and true. If it is not, it will return the zero value of the type and
// false.
func (p *Prop[T]) Static() (value T, ok bool) {
	if !p.isStatic {
		return
	}

	return p.static, true
}

// SetLogger sets the logger for the property. This is useful for debugging
// purposes.
func (p *Prop[T]) SetLogger(log zerolog.Logger) {
	if p == nil {
		return
	}

	// We want to be careful not to spam the user
	sampler := &zerolog.BurstSampler{
		Burst:       5,
		Period:      1 * time.Second,
		NextSampler: &zerolog.BasicSampler{N: 100},
	}
	p.log = log.Sample(sampler)
}

// Presettable is a general interface for providing preset values to many
// properties at once.
type Presettable interface {
	Preset(
		ctx context.Context,
		context interface{},
		params ...interface{},
	)
	SetLogger(log zerolog.Logger)
	ClearCache()
}

// SetTTL sets the time-to-live for the property. This is the amount of time to
// wait before recalculating the property.
func (p *Prop[T]) SetTTL(ttl time.Duration) {
	if p == nil {
		return
	}

	p.ttl = ttl
}

// ClearCache clears the cache of the property, forcing it to be recalculated
// on the next call to Get if necessary.
func (p *Prop[T]) ClearCache() {
	if p == nil {
		return
	}

	p.cacheExpires = time.Time{}
	var zero T
	p.cachedValue = zero
}

// Preset sets the context and parameters for the property ahead of time. This
// is useful for properties that are dynamic and need to be calculated with a
// specific context and parameters.
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
	p.ClearCache()
}

// Get returns the value of the property. If the property is static, it will
// return the value and true. If the property is dynamic, it will call the
// function with the context and parameters and return the result.
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

	if p.cacheExpires.After(time.Now()) {
		return p.cachedValue, true
	}

	janetValue, err := p.dynamic.CallResult(
		ctx,
		context,
		janet.Params{},
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

	if !ok {
		return
	}

	p.cachedValue = value
	p.cacheExpires = time.Now().Add(p.ttl)
	return
}

// GetPreset returns the value of the property, computing it using the preset
// values provided in the Preset method.
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

// NewStatic creates a new static property with the given value.
func NewStatic[T any](value T) *Prop[T] {
	return &Prop[T]{
		isStatic: true,
		static:   value,
	}
}

type String = Prop[string]
type Color = Prop[*style.Color]
type Border = Prop[*style.Border]
type ColorMap = Prop[*style.ColorMap]
