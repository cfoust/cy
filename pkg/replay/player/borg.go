package player

import (
	"compress/gzip"
	"context"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/sessions"

	"github.com/ugorji/go/codec"
)

type borgHeader struct {
	Version int
}

type countingReader struct {
	r io.Reader
	n int64
}

func (c *countingReader) Read(p []byte) (int, error) {
	n, err := c.r.Read(p)
	c.n += int64(n)
	return n, err
}

// FromBorgContext loads a .borg file from disk, processing events as it reads.
// If `progress` is not nil, it receives percent updates from 0-100 and is
// closed once loading completes.
func FromBorgContext(
	ctx context.Context,
	path string,
	progress chan<- int,
) (p *Player, err error) {
	if progress != nil {
		defer close(progress)
	}

	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer func() { _ = f.Close() }()

	stat, err := f.Stat()
	if err != nil {
		return nil, err
	}
	totalBytes := stat.Size()
	if totalBytes <= 0 {
		totalBytes = 1
	}

	reader := &countingReader{r: f}

	gz, err := gzip.NewReader(reader)
	if err != nil {
		return nil, fmt.Errorf("gzip error: %w", err)
	}
	defer func() { _ = gz.Close() }()

	handle := new(codec.MsgpackHandle)
	decoder := codec.NewDecoder(gz, handle)

	var h borgHeader
	if err := decoder.Decode(&h); err != nil {
		return nil, fmt.Errorf("could not decode header: %w", err)
	}

	if h.Version != sessions.SESSION_FILE_VERSION {
		return nil, fmt.Errorf(
			"header version %d did not match %d",
			h.Version,
			sessions.SESSION_FILE_VERSION,
		)
	}

	sendProgress := func(percent int) {
		if progress == nil {
			return
		}
		select {
		case <-ctx.Done():
			return
		case progress <- percent:
		}
	}

	p = New()

	percent := -1
	updateProgress := func() {
		newPercent := int(float64(reader.n) / float64(totalBytes) * 100)
		// The file can grow while we're reading it (e.g. a live recording),
		// which can cause this estimate to exceed 100%.
		newPercent = geom.Clamp(newPercent, 0, 99)
		if newPercent == percent {
			return
		}
		percent = newPercent
		sendProgress(percent)
	}

	for {
		if ctx.Err() != nil {
			return nil, ctx.Err()
		}

		var stamp time.Time
		err := decoder.Decode(&stamp)
		if err == io.EOF || err == io.ErrUnexpectedEOF {
			break
		}
		if err != nil {
			return nil, err
		}

		var type_ P.MessageType
		err = decoder.Decode(&type_)
		if err == io.EOF || err == io.ErrUnexpectedEOF {
			break
		}
		if err != nil {
			return nil, err
		}

		var msg P.Message
		switch type_ {
		case P.MessageTypeOutput:
			var data []byte
			if err := decoder.Decode(&data); err != nil {
				if err == io.EOF || err == io.ErrUnexpectedEOF {
					break
				}
				return nil, err
			}
			msg = P.OutputMessage{Data: data}
		case P.MessageTypeSize:
			size := P.SizeMessage{}
			if err := decoder.Decode(&size); err != nil {
				if err == io.EOF || err == io.ErrUnexpectedEOF {
					break
				}
				return nil, err
			}
			msg = size
		default:
			// Unrecognized message types are ignored.
			continue
		}

		if err := p.Process(sessions.Event{
			Stamp:   stamp,
			Message: msg,
		}); err != nil {
			return nil, err
		}

		updateProgress()
	}

	sendProgress(100)
	return p, nil
}
