package cmd

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/replay"
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

	var stream mux.Stream = cmd
	if len(borgPath) > 0 {
		recorder, err := sessions.NewFileRecorder(ctx, borgPath)
		if err != nil {
			return nil, err
		}
		stream = sessions.NewEventStream(cmd, recorder)
	}

	replayable := replay.NewReplayable(ctx, stream, replayBinds)
	return replayable, nil
}
