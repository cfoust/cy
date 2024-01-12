package sessions

import (
	"encoding/json"
	"fmt"
	"os"

	P "github.com/cfoust/cy/pkg/io/protocol"
)

// Write `events` to the given file with path `filename` in the Asciicast v2
// format. See https://docs.asciinema.org/manual/asciicast/v2/ for more
// information.
func WriteAsciinema(filename string, events []Event) error {
	f, err := os.Create(filename)
	if err != nil {
		return err
	}

	// First write the header
	data, err := json.Marshal(map[string]interface{}{
		"version": 2,
		// Theoretically we could derive these, but who cares?
		"width":  80,
		"height": 26,
	})
	if err != nil {
		return err
	}
	_, err = f.Write(data)
	if err != nil {
		return err
	}
	_, err = f.Write([]byte("\n"))
	if err != nil {
		return err
	}

	if len(events) == 0 {
		return nil
	}

	startTime := events[0].Stamp

	// Then write the events
	for _, event := range events {
		stamp := event.Stamp.Sub(startTime).Seconds()

		switch event := event.Message.(type) {
		case P.OutputMessage:
			data, err := json.Marshal([]interface{}{
				stamp,
				"o",
				string(event.Data),
			})
			if err != nil {
				return err
			}
			_, err = f.Write(data)
			if err != nil {
				return err
			}
		case P.SizeMessage:
			data, err := json.Marshal([]interface{}{
				stamp,
				"r",
				fmt.Sprintf("%dx%d", event.Columns, event.Rows),
			})
			if err != nil {
				return err
			}
			_, err = f.Write(data)
			if err != nil {
				return err
			}
		}

		_, err = f.Write([]byte("\n"))
		if err != nil {
			return err
		}
	}

	err = f.Close()
	if err != nil {
		return err
	}
	return nil
}
