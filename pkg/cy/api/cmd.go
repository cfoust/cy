package api

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	C "github.com/cfoust/cy/pkg/cmd"
	"github.com/cfoust/cy/pkg/cy/cmd"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/replay/detect"
	"github.com/cfoust/cy/pkg/replay/replayable"
	"github.com/cfoust/cy/pkg/util"
)

type CmdParams struct {
	Command string
	Args    []string
	Name    string
	Path    string
	Restart bool
}

type ExecParams struct {
	Path    string
	Env     *janet.Value
	Stdin   string
	Timeout *int
}

type ExecResult struct {
	Stdout   string
	Stderr   string
	ExitCode int
}

type CommandStore interface {
	OpenDatabases([]string) error
	QueryCommands(context.Context) ([]C.CommandEvent, error)
}

type CmdModule struct {
	Server               Server
	Store                CommandStore
	Lifetime             util.Lifetime
	Tree                 *tree.Tree
	TimeBinds, CopyBinds *bind.BindScope
}

func (c *CmdModule) New(
	user any,
	groupId *janet.Value,
	cmdParams *janet.Named[CmdParams],
) (tree.NodeID, error) {
	defer groupId.Free()

	group, err := resolveGroup(c.Tree, groupId)
	if err != nil {
		return 0, err
	}

	command := "/bin/bash"
	if client, ok := user.(Client); ok {
		command = client.Params().DefaultShell()
	}

	values := cmdParams.WithDefault(CmdParams{
		Command: command,
	})

	id, create := group.NewPaneCreator(c.Lifetime.Ctx())

	replayable, err := cmd.New(
		c.Lifetime.Ctx(),
		stream.CmdOptions{
			Command:   values.Command,
			Args:      values.Args,
			Directory: values.Path,
			Restart:   values.Restart,
			Env: map[string]string{
				"CY": fmt.Sprintf(
					"%s:%d",
					c.Server.SocketName(),
					id.Id(),
				),
			},
		},
		id.Params(),
		c.TimeBinds,
		c.CopyBinds,
	)
	if err != nil {
		return 0, err
	}

	pane := create(replayable)

	if values.Name != "" {
		pane.SetName(values.Name)
	}

	return pane.Id(), nil
}

func resolveCmd(
	tree *tree.Tree,
	id *janet.Value,
) (*stream.Cmd, error) {
	pane, err := resolvePane(tree, id)
	if err != nil {
		return nil, err
	}

	r, ok := pane.Screen().(*replayable.Replayable)
	if !ok {
		return nil, fmt.Errorf("pane was not a cmd")
	}

	cmd, ok := r.Cmd().(*stream.Cmd)
	if !ok {
		return nil, fmt.Errorf("pane was not a cmd")
	}

	return cmd, nil
}

func (c *CmdModule) Path(id *janet.Value) (*string, error) {
	defer id.Free()

	cmd, err := resolveCmd(c.Tree, id)
	if err != nil {
		return nil, err
	}

	path, err := cmd.Path()
	if err != nil {
		return nil, err
	}

	return &path, nil
}

func (c *CmdModule) Pid(id *janet.Value) (*int, error) {
	defer id.Free()

	cmd, err := resolveCmd(c.Tree, id)
	if err != nil {
		return nil, err
	}

	pid, err := cmd.Pid()
	if err != nil {
		return nil, err
	}

	return &pid, nil
}

func (c *CmdModule) Kill(id *janet.Value) error {
	defer id.Free()

	cmd, err := resolveCmd(c.Tree, id)
	if err != nil {
		return err
	}

	cmd.Kill()
	return nil
}

func (c *CmdModule) Commands(id *janet.Value) (*[]detect.Command, error) {
	defer id.Free()

	pane, err := resolvePane(c.Tree, id)
	if err != nil {
		return nil, err
	}

	r, ok := pane.Screen().(*replayable.Replayable)
	if !ok {
		return nil, fmt.Errorf("pane was not a cmd")
	}

	commands := r.Commands()
	return &commands, nil
}

func (c *CmdModule) Title(id *janet.Value) (*string, error) {
	defer id.Free()

	pane, err := resolvePane(c.Tree, id)
	if err != nil {
		return nil, err
	}

	r, ok := pane.Screen().(*replayable.Replayable)
	if !ok {
		return nil, fmt.Errorf("pane was not a cmd")
	}

	terminal, ok := r.Screen().(*screen.Terminal)
	if !ok {
		return nil, fmt.Errorf("pane does not have a terminal")
	}

	title := terminal.Title()
	return &title, nil
}

func (c *CmdModule) Query() ([]C.CommandEvent, error) {
	// We need to aggregate all of the data directories to which _all_
	// panes could be recording and open any existing databases there
	// before querying. The command dataabase is otherwise opened only on
	// demand when a command is stored.
	dataDirs := make(map[string]struct{})
	for _, leaf := range c.Tree.Leaves() {
		dataDirs[leaf.Params().DataDirectory()] = struct{}{}
	}

	dirs := make([]string, 0, len(dataDirs))
	for dir := range dataDirs {
		dirs = append(dirs, dir)
	}

	err := c.Store.OpenDatabases(dirs)
	if err != nil {
		return nil, err
	}

	commands, err := c.Store.QueryCommands(c.Lifetime.Ctx())
	if err != nil {
		return nil, err
	}

	// All timestamps returned by the database are in UTC, so we need to
	// convert to the "local" timezone
	for i, command := range commands {
		commands[i].ExecutedAt = command.ExecutedAt.Local()
		commands[i].CompletedAt = command.CompletedAt.Local()
	}

	return commands, nil
}

func (c *CmdModule) Exec(
	ctx context.Context,
	command string,
	args []string,
	params *janet.Named[ExecParams],
) (ExecResult, error) {
	values := params.Values()

	execCtx := ctx
	if values.Timeout != nil && *values.Timeout > 0 {
		var cancel context.CancelFunc
		execCtx, cancel = context.WithTimeout(
			ctx,
			time.Duration(*values.Timeout)*time.Second,
		)
		defer cancel()
	}

	cmd := exec.CommandContext(execCtx, command, args...)

	if values.Path != "" {
		cmd.Dir = values.Path
	}

	cmd.Env = os.Environ()
	cmd.Env = append(cmd.Env, "CY="+c.Server.SocketName())

	if values.Env != nil {
		var envMap map[string]string
		err := values.Env.Unmarshal(&envMap)
		values.Env.Free()
		if err != nil {
			return ExecResult{}, fmt.Errorf(
				"invalid env: %w",
				err,
			)
		}
		for k, v := range envMap {
			cmd.Env = append(cmd.Env, k+"="+v)
		}
	}

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	if values.Stdin != "" {
		cmd.Stdin = strings.NewReader(values.Stdin)
	}

	err := cmd.Run()

	result := ExecResult{
		Stdout: stdout.String(),
		Stderr: stderr.String(),
	}

	if err != nil {
		if execCtx.Err() == context.DeadlineExceeded {
			return result, fmt.Errorf("command timed out")
		}

		if exitErr, ok := err.(*exec.ExitError); ok {
			result.ExitCode = exitErr.ExitCode()
			return result, nil
		}

		return result, err
	}

	return result, nil
}
