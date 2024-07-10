package cy

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/toasts"

	"github.com/rs/zerolog"
)

import _ "embed"

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

	return toasts.ToastLevelError, fmt.Errorf("you must provide one of :info, :warn, or :error")
}

type CyModule struct {
	cy *Cy
}

var _ janet.Documented = (*CyModule)(nil)

//go:embed docs-cy.md
var DOCS_CY string

func (i *CyModule) Documentation() string {
	return DOCS_CY
}

func (c *CyModule) KillServer() {
	c.cy.Shutdown()
}

func (c *CyModule) Detach(user interface{}) {
	client, ok := user.(*Client)
	if !ok {
		return
	}

	client.Detach("detached")
}

func (c *CyModule) ReloadConfig() error {
	return c.cy.reloadConfig()
}

func (c *CyModule) Paste(user interface{}) {
	client, ok := user.(*Client)
	if !ok {
		return
	}

	buffer := client.buffer
	if len(buffer) == 0 {
		return
	}

	client.binds.Input([]byte(buffer))
}

func (c *CyModule) Toast(context interface{}, level *janet.Value, message string) error {
	defer level.Free()

	toastLevel, err := resolveLevel(level)
	if err != nil {
		return err
	}

	client, ok := context.(*Client)
	if !ok {
		return fmt.Errorf("unable to detect client")
	}

	client.toaster.Send(toasts.Toast{
		Level:   toastLevel,
		Message: message,
	})
	return nil
}

func (c *CyModule) Log(level *janet.Value, text string) error {
	defer level.Free()

	levelValue, err := resolveLevel(level)
	if err != nil {
		return err
	}

	var logLevel zerolog.Level = zerolog.InfoLevel
	switch levelValue {
	case toasts.ToastLevelInfo:
		logLevel = zerolog.InfoLevel
	case toasts.ToastLevelWarn:
		logLevel = zerolog.WarnLevel
	case toasts.ToastLevelError:
		logLevel = zerolog.ErrorLevel
	}

	c.cy.log.WithLevel(logLevel).Msgf(text)
	return nil
}
