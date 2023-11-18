package cy

import (
	"github.com/cfoust/cy/pkg/cy/params"
)

func (c *Cy) setDefaults(options Options) error {
	defaults := map[string]interface{}{
		params.ParamDataDirectory: options.DataDir,
		params.ParamAnimate:       true,
	}

	for key, value := range defaults {
		err := c.defaults.Set(key, value)
		if err != nil {
			return err
		}
	}
	return nil
}
