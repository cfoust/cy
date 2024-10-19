package api

import (
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
)

type ParamModule struct {
	Tree   *tree.Tree
	Server Server
}

// haha
type ParamParams struct {
	Target *janet.Value
}

// isClientTarget reports whether value is the :client keyword.
func isClientTarget(value *janet.Value) bool {
	return value.Unmarshal(&KEYWORD_CLIENT) == nil
}

func (p *ParamModule) Get(
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

	node, err := resolveNode(p.Tree, params.Target)
	if err != nil {
		return nil, err
	}

	value, _ := node.Params().Get(string(keyword))
	return value, nil
}

func (p *ParamModule) Set(
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
