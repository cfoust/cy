package cmd

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/replay"
	"github.com/cfoust/cy/pkg/sessions"
)

func New(
	ctx context.Context,
	options stream.CmdOptions,
	dataDir string,
	replayBinds, copyBinds *bind.BindScope,
) (*replay.Replayable, error) {
	cmd, err := stream.NewCmd(ctx, options, geom.DEFAULT_SIZE)
	if err != nil {
		return nil, err
	}

	if len(dataDir) == 0 {
		replayable := replay.NewReplayable(
			ctx,
			cmd,
			cmd,
			replayBinds,
			copyBinds,
		)
		return replayable, nil
	}

	borgPath, err := sessions.GetFilename(dataDir, options.Directory)
	if err != nil {
		return nil, err
	}

	recorder, err := sessions.NewFileRecorder(ctx, borgPath)
	if err != nil {
		return nil, err
	}

	return replay.NewReplayable(
		ctx,
		cmd,
		sessions.NewEventStream(cmd, recorder),
		replayBinds,
		copyBinds,
	), nil
}
