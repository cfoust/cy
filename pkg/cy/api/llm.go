package api

import (
	"context"

	"github.com/tmc/langchaingo/llms"
	"github.com/tmc/langchaingo/llms/openai"
)

type LLMModule struct {
}

func (l *LLMModule) Complete(ctx context.Context, prompt string) (string, error) {
	llm, err := openai.New()
	if err != nil {
		return "", err
	}

	completion, err := llms.GenerateFromSinglePrompt(ctx, llm, prompt)
	if err != nil {
		return "", err
	}

	return completion, nil
}
