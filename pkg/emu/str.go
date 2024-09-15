package emu

import (
	"fmt"
	"math"
	"regexp"
	"strconv"
	"strings"
)

// STR sequences are similar to CSI sequences, but have string arguments (and
// as far as I can tell, don't really have a name; STR is the name I took from
// suckless which I imagine comes from rxvt or xterm).
type strEscape struct {
	typ  rune
	buf  []rune
	args []string
}

func (s *strEscape) reset() {
	s.typ = 0
	s.buf = s.buf[:0]
	s.args = nil
}

func (s *strEscape) put(c rune) {
	// TODO: improve allocs with an array backed slice; bench first
	if len(s.buf) < 256 {
		s.buf = append(s.buf, c)
	}
	// Going by st, it is better to remain silent when the STR sequence is not
	// ended so that it is apparent to users something is wrong. The length sanity
	// check ensures we don't absorb the entire stream into memory.
	// TODO: see what rxvt or xterm does
}

func (s *strEscape) parse() {
	s.args = strings.Split(string(s.buf), ";")
}

func (s *strEscape) arg(i, def int) int {
	if i >= len(s.args) || i < 0 {
		return def
	}
	i, err := strconv.Atoi(s.args[i])
	if err != nil {
		return def
	}
	return i
}

func (s *strEscape) argString(i int, def string) string {
	if i >= len(s.args) || i < 0 {
		return def
	}
	return s.args[i]
}

func (t *State) handleSTR() {
	s := &t.str
	s.parse()
}

func (t *State) setColorName(j Color, p *string) error {
	if p == nil {
		// restore color
		delete(t.colorOverride, j)
	} else {
		// set color
		r, g, b, err := parseColor(*p)
		if err != nil {
			return err
		}
		t.colorOverride[j] = RGBColor(r, g, b)
	}

	return nil
}

func (t *State) oscColorResponse(j Color, num int) {
	k, ok := t.colorOverride[j]
	if ok {
		j = k
	}

	r, g, b, ok := j.RGB()
	if !ok {
		return
	}
	t.w.Write([]byte(fmt.Sprintf("\033]%d;rgb:%02x%02x/%02x%02x/%02x%02x\007", num, r, r, g, g, b, b)))
}

func (t *State) osc4ColorResponse(j Color) {
	if j < 0 {
		t.logf("failed to fetch osc4 color %d\n", j)
		return
	}

	k, ok := t.colorOverride[j]
	if ok {
		j = k
	}

	r, g, b, ok := j.RGB()
	if !ok {
		return
	}
	t.w.Write([]byte(fmt.Sprintf("\033]4;%d;rgb:%02x%02x/%02x%02x/%02x%02x\007", j, r, r, g, g, b, b)))
}

var (
	RGBPattern  = regexp.MustCompile(`^([\da-f]{1})\/([\da-f]{1})\/([\da-f]{1})$|^([\da-f]{2})\/([\da-f]{2})\/([\da-f]{2})$|^([\da-f]{3})\/([\da-f]{3})\/([\da-f]{3})$|^([\da-f]{4})\/([\da-f]{4})\/([\da-f]{4})$`)
	HashPattern = regexp.MustCompile(`[\da-f]`)
)

func parseColor(p string) (r, g, b int, err error) {
	if len(p) == 0 {
		err = fmt.Errorf("empty color spec")
		return
	}

	low := strings.ToLower(p)
	if strings.HasPrefix(low, "rgb:") {
		low = low[4:]
		sm := RGBPattern.FindAllStringSubmatch(low, -1)
		if len(sm) != 1 || len(sm[0]) == 0 {
			err = fmt.Errorf("invalid rgb color spec: %s", p)
			return
		}
		m := sm[0]

		var base float64
		if len(m[1]) > 0 {
			base = 15
		} else if len(m[4]) > 0 {
			base = 255
		} else if len(m[7]) > 0 {
			base = 4095
		} else {
			base = 65535
		}

		r64, err := strconv.ParseInt(firstNonEmpty(m[1], m[4], m[7], m[10]), 16, 0)
		if err != nil {
			return r, g, b, err
		}

		g64, err := strconv.ParseInt(firstNonEmpty(m[2], m[5], m[8], m[11]), 16, 0)
		if err != nil {
			return r, g, b, err
		}

		b64, err := strconv.ParseInt(firstNonEmpty(m[3], m[6], m[9], m[12]), 16, 0)
		if err != nil {
			return r, g, b, err
		}

		r = int(math.Round(float64(r64) / base * 255))
		g = int(math.Round(float64(g64) / base * 255))
		b = int(math.Round(float64(b64) / base * 255))
		return r, g, b, nil
	} else if strings.HasPrefix(low, "#") {
		low = low[1:]
		m := HashPattern.FindAllString(low, -1)
		if !oneOf(len(m), []int{3, 6, 9, 12}) {
			err = fmt.Errorf("invalid hash color spec: %s", p)
			return
		}

		adv := len(low) / 3
		for i := 0; i < 3; i++ {
			c, err := strconv.ParseInt(low[adv*i:adv*i+adv], 16, 0)
			if err != nil {
				return r, g, b, err
			}

			var v int64
			switch adv {
			case 1:
				v = c << 4
			case 2:
				v = c
			case 3:
				v = c >> 4
			default:
				v = c >> 8
			}

			switch i {
			case 0:
				r = int(v)
			case 1:
				g = int(v)
			case 2:
				b = int(v)
			}
		}
		return
	} else {
		err = fmt.Errorf("invalid color spec: %s", p)
		return
	}
}

func maybe(p *string) string {
	if p == nil {
		return "<nil>"
	}
	return *p
}

func firstNonEmpty(strs ...string) string {
	if len(strs) == 0 {
		return ""
	}
	for _, str := range strs {
		if len(str) > 0 {
			return str
		}
	}
	return strs[len(strs)-1]
}

func oneOf(v int, is []int) bool {
	for _, i := range is {
		if v == i {
			return true
		}
	}
	return false
}
