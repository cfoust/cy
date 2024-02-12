package cmd

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux/screen/replay"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/sessions"
)

func New(
	ctx context.Context,
	options stream.CmdOptions,
	dataDir string,
	replayBinds *bind.BindScope,
) (*replay.Replayable, error) {
	cmd, err := stream.NewCmd(
		ctx,
		options,
		geom.DEFAULT_SIZE,
	)
	if err != nil {
		return nil, err
	}

	var borgPath string
	if len(dataDir) > 0 {
		borgPath, err = sessions.GetFilename(dataDir, options.Directory)
		if err != nil {
			return nil, err
		}
	}

	// TODO(cfoust): 02/12/24 handle empty path
	recorder, err := sessions.NewFileRecorder(ctx, borgPath)
	if err != nil {
		return nil, err
	}

	replayable := replay.NewReplayable(
		ctx,
		sessions.NewEventStream(cmd, recorder),
		replayBinds,
	)
	return replayable, nil
}
