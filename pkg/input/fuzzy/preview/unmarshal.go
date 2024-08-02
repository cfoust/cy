package preview

import (
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
)

var (
	KEYWORD_ANIMATION  = janet.Keyword("animation")
	KEYWORD_FRAME      = janet.Keyword("frame")
	KEYWORD_LAYOUT     = janet.Keyword("layout")
	KEYWORD_NODE       = janet.Keyword("node")
	KEYWORD_REPLAY     = janet.Keyword("replay")
	KEYWORD_SCROLLBACK = janet.Keyword("scrollback")
	KEYWORD_TEXT       = janet.Keyword("text")
)

type previewInput struct {
	Type janet.Keyword
}

func Unmarshal(input *janet.Value) (result interface{}, err error) {
	preview := previewInput{}
	err = input.Unmarshal(&preview)
	if err != nil {
		return
	}

	switch preview.Type {
	case KEYWORD_TEXT:
		text := TextType{}
		err = input.Unmarshal(&text)
		if err != nil {
			return
		}
		result = text
	case KEYWORD_NODE:
		node := NodeType{}
		err = input.Unmarshal(&node)
		if err != nil {
			return
		}
		result = node
		return
	case KEYWORD_REPLAY:
		replay := ReplayType{}
		err = input.Unmarshal(&replay)
		if err != nil {
			return
		}
		result = replay
	case KEYWORD_SCROLLBACK:
		scrollback := ScrollbackType{}
		err = input.Unmarshal(&scrollback)
		if err != nil {
			return
		}
		result = scrollback
	case KEYWORD_FRAME:
		frame := FrameType{}
		err = input.Unmarshal(&frame)
		if err != nil {
			return
		}
		result = frame
	case KEYWORD_ANIMATION:
		animation := AnimationType{}
		err = input.Unmarshal(&animation)
		if err != nil {
			return
		}
		result = animation
	case KEYWORD_LAYOUT:
		layout := LayoutType{}
		err = input.Unmarshal(&layout)
		if err != nil {
			return
		}
		result = layout
	default:
		err = fmt.Errorf("invalid preview type %s", preview.Type)
		return
	}

	return
}
