package cmd

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
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

	var borgPath string
	if len(dataDir) > 0 {
		borgPath, err = sessions.GetFilename(dataDir, options.Directory)
		if err != nil {
			return nil, err
		}
	}

	recorder, err := sessions.NewRecorder(ctx, borgPath, cmd)
	if err != nil {
		return nil, err
	}

	replayable := replayable.New(
		ctx,
		cmd,
		recorder,
		replayBinds,
	)
	return replayable, nil
}
