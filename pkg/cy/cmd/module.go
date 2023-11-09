package cmd

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/replayable"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/sessions"
)

func New(
	ctx context.Context,
	options stream.CmdOptions,
	dataDir string,
	replayBinds *bind.BindScope,
) (*replayable.Replayable, error) {
	cmd, err := stream.NewCmd(
		ctx,
		options,
		geom.DEFAULT_SIZE,
	)
	if err != nil {
		return nil, err
	}

	borgPath, err := sessions.GetFilename(dataDir, options.Directory)
	if err != nil {
		return nil, err
	}

	recorder, err := sessions.NewRecorder(ctx, borgPath, cmd)
	if err != nil {
		return nil, err
	}

	terminal := screen.NewTerminal(ctx, recorder, geom.DEFAULT_SIZE)
	replayable := replayable.New(
		ctx,
		terminal,
		cmd,
		recorder,
		replayBinds,
	)
	return replayable, nil
}
