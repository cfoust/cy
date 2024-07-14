package vtparser

import (
	"math"

	"github.com/danielgatis/go-vte/utf8"
)

// State represents a state type
type State = byte

// Action represents a action type
type Action = byte

const maxIntermediates = 2
const maxOscRaw = 1024
const maxParams = 16

type printCallback func(char rune)
type execCallback func(b byte)
type putCallback func(b byte)
type unhookCallback func()
type hookCallback func(params []int64, intermediates []byte, ignore bool, r rune)
type oscCallback func(params [][]byte, bellTerminated bool)
type csiCallback func(params []int64, intermediates []byte, ignore bool, r rune)
type escCallback func(intermediates []byte, ignore bool, b byte)

// Parser represents a state machine.
type Parser struct {
	state           byte
	intermediates   [maxIntermediates]byte
	intermediateIdx int
	params          [maxParams]int64
	param           int64
	numParams       int
	oscRaw          []byte
	oscParams       [maxParams][2]int
	oscNumParams    int
	ignoring        bool
	utf8Parser      *utf8.Parser

	prtcb printCallback
	execb execCallback
	putcb putCallback
	uhocb unhookCallback
	hokcb hookCallback
	osccb oscCallback
	csicb csiCallback
	esccb escCallback
}

// New returns a new parser.
func New(
	prtcb printCallback,
	execb execCallback,
	putcb putCallback,
	uhocb unhookCallback,
	hokcb hookCallback,
	osccb oscCallback,
	csicb csiCallback,
	esccb escCallback,
) *Parser {
	p := &Parser{
		state: groundState,

		prtcb: prtcb,
		execb: execb,
		putcb: putcb,
		uhocb: uhocb,
		hokcb: hokcb,
		osccb: osccb,
		csicb: csicb,
		esccb: esccb,
	}

	p.utf8Parser = utf8.New(
		func(r rune) {
			p.prtcb(r)
			p.state = groundState
		},

		func() {
			p.prtcb('�')
			p.state = groundState
		},
	)

	return p
}

// Advance advances the state machine.
func (p *Parser) Advance(b byte) {
	if p.state == utf8State {
		p.utf8Parser.Advance(b)
	} else {
		change := stateTable[anywhereState][b]

		if change == 0 {
			change = stateTable[p.state][b]
		}

		state := change & 0x0f
		action := change >> 4

		p.performStateChange(state, action, b)
	}
}

// Intermediates returns the intermediates
func (p *Parser) Intermediates() []byte {
	return p.intermediates[:p.intermediateIdx]
}

// Params returns the params
func (p *Parser) Params() []int64 {
	return p.params[:p.numParams]
}

// OscParams returns the osc params
func (p *Parser) OscParams() [][]byte {
	params := make([][]byte, 0)

	for i := 0; i < p.oscNumParams; i++ {
		indices := p.oscParams[i]
		param := p.oscRaw[indices[0]:indices[1]]
		params = append(params, param)
	}

	return params
}

// StateName returns the current state name
func (p *Parser) StateName() string {
	return stateNames[p.state]
}

// State returns the current state
func (p *Parser) State() State {
	return p.state
}

func (p *Parser) performStateChange(state, action, b byte) {
	if state == anywhereState {
		p.performAction(action, b)
	} else {
		exitAction := exitActions[p.state]
		entryAction := entryActions[state]

		if exitAction != 0 {
			p.performAction(exitAction, b)
		}

		if action != 0 {
			p.performAction(action, b)
		}

		if entryAction != 0 {
			p.performAction(entryAction, b)
		}

		p.state = state
	}
}

func (p *Parser) performAction(action, b byte) {
	switch action {
	case ignoreAction:
		break

	case noneAction:
		break

	case printAction:
		p.prtcb(rune(b))

	case executeAction:
		p.execb(b)

	case hookAction:
		if p.numParams == maxParams {
			p.ignoring = true
		} else {
			p.params[p.numParams] = p.param
			p.numParams++
		}

		p.hokcb(
			p.Params(),
			p.Intermediates(),
			p.ignoring,
			rune(b),
		)

	case putAction:
		p.putcb(b)

	case oscStartAction:
		p.oscRaw = make([]byte, 0)
		p.oscNumParams = 0

	case oscPutAction:
		idx := len(p.oscRaw)

		if b == ';' {
			paramIdx := p.oscNumParams
			switch paramIdx {
			case maxParams:
				return
			case 0:
				p.oscParams[paramIdx] = [2]int{0, idx}
				p.oscNumParams++
			default:
				prev := p.oscParams[paramIdx-1]
				begin := prev[1]
				p.oscParams[paramIdx] = [2]int{begin, idx}
				p.oscNumParams++
			}
		} else {
			p.oscRaw = append(p.oscRaw, b)
		}

	case oscEndAction:
		paramIdx := p.oscNumParams
		idx := len(p.oscRaw)

		switch paramIdx {
		case maxParams:
			break
		case 0:
			p.oscParams[paramIdx] = [2]int{0, idx}
			p.oscNumParams++
		default:
			prev := p.oscParams[paramIdx-1]
			begin := prev[1]
			p.oscParams[paramIdx] = [2]int{begin, idx}
			p.oscNumParams++
		}

		p.osccb(
			p.OscParams(),
			b == 0x07,
		)

	case unhookAction:
		p.uhocb()

	case csiDispatchAction:
		if p.numParams == maxParams {
			p.ignoring = true
		} else {
			p.params[p.numParams] = p.param
			p.numParams++
		}

		p.csicb(
			p.Params(),
			p.Intermediates(),
			p.ignoring,
			rune(b),
		)

	case escDispatchAction:
		p.esccb(
			p.Intermediates(),
			p.ignoring,
			b,
		)

	case collectAction:
		if p.intermediateIdx == maxIntermediates {
			p.ignoring = true
		} else {
			p.intermediates[p.intermediateIdx] = b
			p.intermediateIdx++
		}

	case paramAction:
		idx := p.numParams

		if idx == maxParams {
			p.ignoring = true
			return
		}

		if b == ';' {
			p.params[idx] = p.param
			p.param = 0
			p.numParams++
		} else {
			p.param = smul64(p.param, 10)
			p.param = sadd64(p.param, int64((b - '0')))
		}

	case clearAction:
		p.intermediateIdx = 0
		p.ignoring = false
		p.numParams = 0
		p.param = 0

	case beginUtf8Action:
		p.utf8Parser.Advance(b)
	}
}

func sadd64(a, b int64) int64 {
	if b > 0 && a > math.MaxInt64-b {
		return math.MaxInt64
	}

	if b < 0 && a < math.MinInt64-b {
		return math.MinInt64
	}

	return a + b
}

func smul64(a, b int64) int64 {
	if a > 0 && b > 0 && a > math.MaxInt64/b {
		return math.MaxInt64
	}

	if a > 0 && b <= 0 && b < math.MinInt64/a {
		return math.MinInt64
	}

	if a <= 0 && b > 0 && a < math.MinInt64/b {
		return math.MinInt64
	}

	if a < 0 && b <= 0 && b < math.MaxInt64/a {
		return math.MaxInt64
	}

	return a * b
}
