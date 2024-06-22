package cy

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/cy/api"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/toasts"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/replay"
	"github.com/cfoust/cy/pkg/util"

	"github.com/rs/zerolog"
)

import _ "embed"

//go:embed cy-boot.janet
var CY_BOOT_FILE []byte

//go:embed docs-cy.md
var DOCS_CY string

var (
	KEYWORD_INFO  = janet.Keyword("info")
	KEYWORD_WARN  = janet.Keyword("warn")
	KEYWORD_ERROR = janet.Keyword("error")
)

func resolveLevel(level *janet.Value) (toasts.ToastLevel, error) {
	err := level.Unmarshal(&KEYWORD_INFO)
	if err == nil {
		return toasts.ToastLevelInfo, nil
	}

	err = level.Unmarshal(&KEYWORD_WARN)
	if err == nil {
		return toasts.ToastLevelWarn, nil
	}

	err = level.Unmarshal(&KEYWORD_ERROR)
	if err == nil {
		return toasts.ToastLevelError, nil
	}

	return toasts.ToastLevelError, fmt.Errorf("you must provide one of :info, :warn, or :error")
}

type CyModule struct {
	cy *Cy
}

var _ janet.Documented = (*CyModule)(nil)

func (i *CyModule) Documentation() string {
	return DOCS_CY
}

func (c *CyModule) KillServer() {
	c.cy.Shutdown()
}

func (c *CyModule) Detach(user interface{}) {
	client, ok := user.(*Client)
	if !ok {
		return
	}

	client.Detach("detached")
}

func (c *CyModule) ReloadConfig() error {
	return c.cy.reloadConfig()
}

func (c *CyModule) Get(user interface{}, key *janet.Value) (interface{}, error) {
	defer key.Free()

	var keyword janet.Keyword
	err := key.Unmarshal(&keyword)
	if err != nil {
		return nil, err
	}

	client, ok := user.(*Client)
	if !ok {
		return nil, fmt.Errorf("missing client context")
	}

	// First check the client's parameters
	value, ok := client.params.Get(string(keyword))
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

func (c *CyModule) Set(user interface{}, key *janet.Value, value *janet.Value) error {
	defer key.Free()

	// If there is no client, this probably means a parameter is being set
	// in cy's startup script.
	var node tree.Node = c.cy.tree.Root()
	if client, ok := user.(*Client); ok {
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

type ReplayParams struct {
	Main     bool
	Copy     bool
	Location *geom.Vec2
}

func (c *CyModule) Replay(
	user interface{},
	named *janet.Named[ReplayParams],
) {
	client, ok := user.(*Client)
	if !ok {
		return
	}

	node := client.Node()
	if node == nil {
		return
	}

	pane, ok := node.(*tree.Pane)
	if !ok {
		return
	}

	var options []replay.Option

	params := named.Values()
	if params.Main {
		options = append(options, replay.WithFlow)
	}

	if params.Copy {
		options = append(options, replay.WithCopyMode)
	}

	if params.Location != nil {
		options = append(options, replay.WithLocation(
			*params.Location,
		))
	}

	r, ok := pane.Screen().(*replay.Replayable)
	if !ok {
		return
	}

	r.EnterReplay(options...)
	// TODO(cfoust): 10/08/23 reattach all clients
	client.Attach(node)
}

func (c *CyModule) Paste(user interface{}) {
	client, ok := user.(*Client)
	if !ok {
		return
	}

	buffer := client.buffer
	if len(buffer) == 0 {
		return
	}

	client.binds.Input([]byte(buffer))
}

func (c *CyModule) Toast(context interface{}, level *janet.Value, message string) error {
	defer level.Free()

	toastLevel, err := resolveLevel(level)
	if err != nil {
		return err
	}

	client, ok := context.(*Client)
	if !ok {
		return fmt.Errorf("unable to detect client")
	}

	client.toaster.Send(toasts.Toast{
		Level:   toastLevel,
		Message: message,
	})
	return nil
}

func (c *CyModule) Log(level *janet.Value, text string) error {
	defer level.Free()

	levelValue, err := resolveLevel(level)
	if err != nil {
		return err
	}

	var logLevel zerolog.Level = zerolog.InfoLevel
	switch levelValue {
	case toasts.ToastLevelInfo:
		logLevel = zerolog.InfoLevel
	case toasts.ToastLevelWarn:
		logLevel = zerolog.WarnLevel
	case toasts.ToastLevelError:
		logLevel = zerolog.ErrorLevel
	}

	c.cy.log.WithLevel(logLevel).Msgf(text)
	return nil
}

func (c *Cy) initJanet(ctx context.Context) (*janet.VM, error) {
	vm, err := janet.New(ctx)
	if err != nil {
		return nil, err
	}

	modules := map[string]interface{}{
		"cmd": &api.CmdModule{
			Lifetime:  util.NewLifetime(c.Ctx()),
			Tree:      c.tree,
			TimeBinds: c.timeBinds,
			CopyBinds: c.copyBinds,
		},
		"cy":    &CyModule{cy: c},
		"exec":  &api.ExecModule{Server: c},
		"group": &api.GroupModule{Tree: c.tree},
		"input": &api.InputModule{Tree: c.tree, Server: c.muxServer},
		"key": &api.KeyModule{
			Tree:      c.tree,
			TimeBinds: c.timeBinds,
			CopyBinds: c.copyBinds,
		},
		"pane": &api.PaneModule{Tree: c.tree},
		"path": &api.PathModule{},
		"replay": &api.ReplayModule{
			Lifetime:  util.NewLifetime(c.Ctx()),
			Tree:      c.tree,
			TimeBinds: c.timeBinds,
			CopyBinds: c.copyBinds,
		},
		"tree":     &api.TreeModule{Tree: c.tree},
		"viewport": &api.ViewportModule{},
	}

	for name, module := range modules {
		err := vm.Module(name, module)
		if err != nil {
			return nil, err
		}
	}

	err = vm.ExecuteCall(ctx, nil, janet.Call{
		Code:       CY_BOOT_FILE,
		SourcePath: "cy-boot.janet",
		Options:    janet.DEFAULT_CALL_OPTIONS,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to execute cy-boot.janet: %s", err.Error())
	}

	return vm, nil
}
