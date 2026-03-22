package emu

import (
	"strconv"
	"strings"
)

// CSI (Control Sequence Introducer)
// ESC+[
type csiEscape struct {
	buf           []byte
	args          []int
	subArgs       [][]int
	intermediates []byte
	mode          byte
	priv          bool
}

func (c *csiEscape) reset() {
	c.buf = c.buf[:0]
	c.args = c.args[:0]
	c.subArgs = c.subArgs[:0]
	c.mode = 0
	c.priv = false
}

func (c *csiEscape) parse() {
	c.mode = c.buf[len(c.buf)-1]
	if len(c.buf) == 1 {
		return
	}
	s := string(c.buf)
	c.args = c.args[:0]
	c.subArgs = c.subArgs[:0]
	if s[0] == '?' {
		c.priv = true
		s = s[1:]
	}
	s = s[:len(s)-1]
	ss := strings.Split(s, ";")
	for _, p := range ss {
		// Handle colon-separated sub-parameters (e.g., "4:3")
		parts := strings.Split(p, ":")
		i, err := strconv.Atoi(parts[0])
		if err != nil {
			break
		}
		c.args = append(c.args, i)

		var subs []int
		for _, sp := range parts[1:] {
			v, err := strconv.Atoi(sp)
			if err != nil {
				break
			}
			subs = append(subs, v)
		}
		c.subArgs = append(c.subArgs, subs)
	}
}

func (c *csiEscape) arg(i, def int) int {
	if i >= len(c.args) || i < 0 {
		return def
	}
	return c.args[i]
}

func (c *csiEscape) intermediate(i int, def byte) byte {
	if i >= len(c.intermediates) || i < 0 {
		return def
	}
	return c.intermediates[i]
}

// maxarg takes the maximum of arg(i, def) and def
func (c *csiEscape) maxarg(i, def int) int {
	return max(c.arg(i, def), def)
}
