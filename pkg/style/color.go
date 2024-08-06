package style

import (
	"github.com/cfoust/cy/pkg/janet"

	"github.com/charmbracelet/lipgloss"
)

type Color struct {
	lipgloss.Color
}

var _ janet.Unmarshalable = (*Color)(nil)

func (c *Color) UnmarshalJanet(value *janet.Value) (err error) {
	var str string
	err = value.Unmarshal(&str)
	if err != nil {
		return err
	}

	// TODO(cfoust): 08/06/24 validate colors?
	c.Color = lipgloss.Color(str)
	return nil
}

var _ janet.Marshalable = (*Color)(nil)

func (c *Color) MarshalJanet() interface{} {
	return string(c.Color)
}
