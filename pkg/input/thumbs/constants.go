package thumbs

// The original patterns found here were taken from the tmux-thumbs project,
// which contains the following license:
//
// MIT License
//
// Copyright (c) 2019 Ferran Basora
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import (
	"regexp"
)

var DefaultPatterns = []*regexp.Regexp{
	// markdown_url
	regexp.MustCompile(`\[[^\]]*\]\(([^)]+)\)`),
	// url
	regexp.MustCompile(`(?P<match>(https?://|git@|git://|ssh://|ftp://|file:///)[^\s]+)`),
	// diff_summary
	regexp.MustCompile(`diff --git a/([.\w\-@~\[\]]+?/[.\w\-@\[\]]+) b/([.\w\-@~\[\]]+?/[.\w\-@\[\]]+)`),
	// diff_a
	regexp.MustCompile(`--- a/([^\s]+)`),
	// diff_b
	regexp.MustCompile(`\+\+\+ b/([^\s]+)`),
	// docker
	regexp.MustCompile(`sha256:([0-9a-f]{64})`),
	// path
	regexp.MustCompile(`(?P<match>([.\w\-@$~\[\]]+)?(/[.\w\-@$\[\]]+)+)`),
	// color
	regexp.MustCompile(`#[0-9a-fA-F]{6}`),
	// uid
	regexp.MustCompile(`[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}`),
	// ipfs
	regexp.MustCompile(`Qm[0-9a-zA-Z]{44}`),
	// sha
	regexp.MustCompile(`[0-9a-f]{7,40}`),
	// ip
	regexp.MustCompile(`\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}`),
	// ipv6
	regexp.MustCompile(`[A-f0-9:]+:+[A-f0-9:]+[%\w\d]+`),
	// address
	regexp.MustCompile(`0x[0-9a-fA-F]+`),
	// number
	regexp.MustCompile(`[0-9]{4,}`),
}

// Default alphabet for generating hints (qwerty layout)
var defaultAlphabet = []rune("asdfqwerzxcvjklmiuopghtybn")
