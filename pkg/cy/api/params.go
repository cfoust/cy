package api

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
)

type ParamModule struct {
	Tree            *tree.Tree
	Server          Server
	PersistentStore *params.PersistentStore
}

// haha
type ParamParams struct {
	Target *janet.Value
}

// isClientTarget reports whether value is the :client keyword.
func isClientTarget(value *janet.Value) bool {
	return value.Unmarshal(&KEYWORD_CLIENT) == nil
}

// isPersistTarget reports whether value is the :persist keyword.
func isPersistTarget(value *janet.Value) bool {
	return value.Unmarshal(&KEYWORD_PERSIST) == nil
}

func (p *ParamModule) Get(
	ctx context.Context,
	context interface{},
	key *janet.Value,
	named *janet.Named[ParamParams],
) (interface{}, error) {
	defer key.Free()

	var keyword janet.Keyword
	err := key.Unmarshal(&keyword)
	if err != nil {
		return nil, err
	}

	params := named.Values()

	// Need to be careful to always free params.Target
	if params.Target != nil {
		defer params.Target.Free()
	}

	if params.Target == nil || isClientTarget(params.Target) {
		client, err := getClient(context)
		if err != nil {
			return nil, err
		}

		value, _ := client.Get(string(keyword))
		return value, nil
	}

	if isPersistTarget(params.Target) {
		if p.PersistentStore == nil {
			return nil, fmt.Errorf("persistent storage is not available")
		}

		value, exists, err := p.PersistentStore.Get(
			ctx,
			string(keyword),
		)
		if err != nil {
			return nil, err
		}
		if !exists {
			return nil, nil
		}
		return value, nil
	}

	node, err := resolveNode(p.Tree, params.Target)
	if err != nil {
		return nil, err
	}

	value, _ := node.Params().Get(string(keyword))
	return value, nil
}

func (p *ParamModule) Set(
	ctx context.Context,
	context interface{},
	target *janet.Value,
	key *janet.Value,
	value *janet.Value,
) error {
	defer key.Free()
	defer target.Free()

	var keyword janet.Keyword
	err := key.Unmarshal(&keyword)
	if err != nil {
		return err
	}

	if isPersistTarget(target) {
		if p.PersistentStore == nil {
			return fmt.Errorf("persistent storage is not available")
		}

		err = p.PersistentStore.Set(
			ctx,
			string(keyword),
			value,
		)
		if err != nil {
			return err
		}
		return nil
	}

	var params *params.Parameters
	if isClientTarget(target) {
		client, err := getClient(context)
		if err != nil {
			return err
		}
		params = client.Params()
	} else {
		node, err := resolveNode(p.Tree, target)
		if err != nil {
			return err
		}
		params = node.Params()
	}

	err = params.Set(string(keyword), value)
	if err != nil {
		return err
	}

	// Many params affect rendering; we need to cause a rerender right
	// away to ensure all client screens remain accurate.
	// TODO(cfoust): 10/20/24 maybe this should be under the layout engine so it's throttled?
	p.Server.RerenderClients()
	return nil
}
