package api

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"strings"

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

type ExecParams struct {
	Env   *janet.Value
	Stdin string
}

type ExecResult struct {
	Stdout   string `janet:"stdout"`
	Stderr   string `janet:"stderr"`
	ExitCode int    `janet:"exit-code"`
}

func (c *CmdModule) Exec(
	ctx context.Context,
	args []string,
	params *janet.Named[ExecParams],
) (ExecResult, error) {
	if len(args) < 1 {
		return ExecResult{}, fmt.Errorf(
			"expected at least 1 argument (the command)",
		)
	}

	command := args[0]
	cmdArgs := args[1:]
	values := params.Values()

	cmd := exec.CommandContext(ctx, command, cmdArgs...)

	// Set up environment with CY socket name
	cmd.Env = os.Environ()
	cmd.Env = append(cmd.Env, fmt.Sprintf("CY=%s", c.Server.SocketName()))

	// Add user-provided environment variables
	if values.Env != nil {
		defer values.Env.Free()

		var envMap map[string]string
		if err := values.Env.Unmarshal(&envMap); err == nil {
			for k, v := range envMap {
				cmd.Env = append(cmd.Env, fmt.Sprintf("%s=%s", k, v))
			}
		}
	}

	// Set up stdin if provided
	if values.Stdin != "" {
		cmd.Stdin = strings.NewReader(values.Stdin)
	}

	// Capture stdout and stderr
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	// Run the command
	err := cmd.Run()

	result := ExecResult{
		Stdout: stdout.String(),
		Stderr: stderr.String(),
	}

	// Extract exit code
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			result.ExitCode = exitErr.ExitCode()
		} else {
			return result, err
		}
	}

	return result, nil
}
