package ui

import (
	"context"
	"sort"
	"strings"

	"github.com/cfoust/cy/pkg/stories"
	"github.com/cfoust/cy/pkg/taro"
)

func New(ctx context.Context, filter string) (*taro.Program, error) {
	filteredStories := make([]stories.Story, 0)
	for _, story := range stories.Stories {
		if !strings.HasPrefix(story.Name, filter) {
			continue
		}
		filteredStories = append(filteredStories, story)
	}

	sort.SliceStable(filteredStories, func(i, j int) bool {
		return filteredStories[i].Name < filteredStories[j].Name
	})

	return NewBrowser(
		ctx,
		filteredStories,
	), nil
}
