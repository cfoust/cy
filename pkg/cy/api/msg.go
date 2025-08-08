package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/toasts"

	"github.com/rs/zerolog"
)

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

	return toasts.ToastLevelError, fmt.Errorf(
		"you must provide one of :info, :warn, or :error",
	)
}

type MsgModule struct {
	Server Server
}

func (m *MsgModule) Toast(
	context interface{},
	level *janet.Value,
	message string,
) error {
	defer level.Free()

	toastLevel, err := resolveLevel(level)
	if err != nil {
		return err
	}

	client, err := getClient(context)
	if err != nil {
		return err
	}

	client.Toast(toasts.Toast{
		Level:   toastLevel,
		Message: message,
	})
	return nil
}

func (m *MsgModule) Log(level *janet.Value, text string) error {
	defer level.Free()

	levelValue, err := resolveLevel(level)
	if err != nil {
		return err
	}

	logLevel := zerolog.InfoLevel
	switch levelValue {
	case toasts.ToastLevelInfo:
		logLevel = zerolog.InfoLevel
	case toasts.ToastLevelWarn:
		logLevel = zerolog.WarnLevel
	case toasts.ToastLevelError:
		logLevel = zerolog.ErrorLevel
	}

	m.Server.Log(logLevel, text)
	return nil
}
