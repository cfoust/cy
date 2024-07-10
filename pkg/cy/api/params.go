package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

type ParamModule struct {
	Tree *tree.Tree
}

func (p *ParamModule) Get(
	context interface{},
	key *janet.Value,
) (interface{}, error) {
	defer key.Free()

	var keyword janet.Keyword
	err := key.Unmarshal(&keyword)
	if err != nil {
		return nil, err
	}

	client, ok := context.(Client)
	if !ok {
		return nil, fmt.Errorf("missing client context")
	}

	// First check the client's parameters
	value, ok := client.Get(string(keyword))
	if ok {
		return value, nil
	}

	// Then those found in the tree
	node := client.Node()
	if node == nil {
		return nil, fmt.Errorf("client was not attached")
	}

	value, ok = node.Params().Get(string(keyword))
	return value, nil
}

func (p *ParamModule) Set(
	context interface{},
	key *janet.Value,
	value *janet.Value,
) error {
	defer key.Free()

	// If there is no client, this probably means a parameter is being set
	// in cy's startup script.
	var node tree.Node = p.Tree.Root()
	if client, ok := context.(Client); ok {
		node = client.Node()
		if node == nil {
			return fmt.Errorf("client was not attached")
		}
	}

	var keyword janet.Keyword
	err := key.Unmarshal(&keyword)
	if err != nil {
		return err
	}

	var str string
	err = value.Unmarshal(&str)
	if err == nil {
		node.Params().Set(string(keyword), str)
		return nil
	}

	var _int int
	err = value.Unmarshal(&_int)
	if err == nil {
		node.Params().Set(string(keyword), _int)
		return nil
	}

	var _bool bool
	err = value.Unmarshal(&_bool)
	if err == nil {
		node.Params().Set(string(keyword), _bool)
		return nil
	}

	return fmt.Errorf("parameter type not supported")
}
