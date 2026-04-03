package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// execJanet connects to the cy server and executes Janet code via RPC.
func execJanet(
	code string,
	startServer bool,
	format OutputFormat,
) ([]byte, error) {
	socketPath, err := getSocketPath(CLI.Socket)
	if err != nil {
		return nil, err
	}

	conn, err := connect(socketPath, startServer)
	if err != nil {
		return nil, err
	}

	response, err := RPC[RPCExecArgs, RPCExecResponse](
		conn,
		RPCExec,
		RPCExecArgs{
			Source: "<tmux-compat>",
			Code:   []byte(code),
			Format: format,
		},
	)
	if err != nil {
		return nil, err
	}

	return response.Data, nil
}

// parseTarget strips tmux window/pane suffixes (e.g. ":0" or ":0.0")
// from a target specification, returning just the session name.
func parseTarget(target string) string {
	if i := strings.IndexByte(target, ':'); i >= 0 {
		return target[:i]
	}
	return target
}

// quoteJanet quotes a string for safe embedding in Janet source code.
func quoteJanet(s string) string {
	s = strings.ReplaceAll(s, `\`, `\\`)
	s = strings.ReplaceAll(s, `"`, `\"`)
	s = strings.ReplaceAll(s, "\n", `\n`)
	s = strings.ReplaceAll(s, "\r", `\r`)
	s = strings.ReplaceAll(s, "\t", `\t`)
	return `"` + s + `"`
}

// firstPaneExpr returns a Janet expression that resolves to the first
// pane in the named session group.
func firstPaneExpr(sessionName string) string {
	return fmt.Sprintf(
		`(first (group/leaves (tree/id :root %s)))`,
		quoteJanet("/"+parseTarget(sessionName)),
	)
}

// tmuxKeyToCy maps a tmux key name to the cy equivalent.
func tmuxKeyToCy(key string) string {
	mappings := map[string]string{
		"Enter":    "enter",
		"Space":    "space",
		"Tab":      "tab",
		"Escape":   "esc",
		"BSpace":   "backspace",
		"DC":       "delete",
		"Up":       "up",
		"Down":     "down",
		"Left":     "left",
		"Right":    "right",
		"Home":     "home",
		"End":      "end",
		"PageUp":   "pgup",
		"PageDown": "pgdown",
		"NPage":    "pgdown",
		"PPage":    "pgup",
		"IC":       "insert",
		"F1":       "f1",
		"F2":       "f2",
		"F3":       "f3",
		"F4":       "f4",
		"F5":       "f5",
		"F6":       "f6",
		"F7":       "f7",
		"F8":       "f8",
		"F9":       "f9",
		"F10":      "f10",
		"F11":      "f11",
		"F12":      "f12",
	}

	if mapped, ok := mappings[key]; ok {
		return mapped
	}

	// Handle C-x patterns (tmux ctrl notation)
	if strings.HasPrefix(key, "C-") {
		return "ctrl+" + strings.ToLower(key[2:])
	}

	// Handle M-x patterns (tmux meta/alt notation)
	if strings.HasPrefix(key, "M-") {
		return "alt+" + strings.ToLower(key[2:])
	}

	return strings.ToLower(key)
}

// bufferFilePath returns the temp file path for a named tmux buffer.
func bufferFilePath(name string) string {
	return filepath.Join(
		os.TempDir(),
		fmt.Sprintf(
			"cy-buffer-%d-%s",
			os.Getuid(),
			name,
		),
	)
}

type formatPart struct {
	isVariable bool
	text       string
}

// parseTmuxFormat parses a tmux format string like
// "#{pane_dead}|#{pane_pid}" into parts.
func parseTmuxFormat(format string) []formatPart {
	var parts []formatPart
	i := 0
	for i < len(format) {
		if i < len(format)-1 && format[i] == '#' {
			if format[i+1] == '{' {
				// #{variable}
				end := strings.IndexByte(format[i:], '}')
				if end >= 0 {
					varName := format[i+2 : i+end]
					parts = append(parts, formatPart{
						isVariable: true,
						text:       varName,
					})
					i += end + 1
					continue
				}
			} else {
				// Single-char format like #I
				parts = append(parts, formatPart{
					isVariable: true,
					text:       string(format[i+1]),
				})
				i += 2
				continue
			}
		}

		// Literal text
		start := i
		for i < len(format) {
			if i < len(format)-1 && format[i] == '#' {
				break
			}
			i++
		}
		if i > start {
			parts = append(parts, formatPart{
				isVariable: false,
				text:       format[start:i],
			})
		}
	}
	return parts
}

// buildFormatJanet converts a parsed tmux format string into a Janet
// (string ...) expression that produces the formatted output for one
// pane. The pane is referenced by the Janet variable "p".
func buildFormatJanet(parts []formatPart) string {
	if len(parts) == 0 {
		return `""`
	}

	var exprs []string
	for _, part := range parts {
		if !part.isVariable {
			exprs = append(exprs, quoteJanet(part.text))
			continue
		}

		switch part.text {
		case "pane_dead":
			exprs = append(exprs,
				`(let [pid (cmd/pid p)`+
					` r (cmd/execute`+
					` @["sh" "-c"`+
					` (string "kill -0 " pid`+
					` " 2>/dev/null")])]`+
					` (if (= 0 (r :exit-code))`+
					` "0" "1"))`)
		case "pane_current_command":
			exprs = append(exprs,
				`(let [pid (cmd/pid p)`+
					` r (cmd/execute`+
					` @["ps" "-o" "comm="`+
					` "-p" (string pid)])]`+
					` (string/trim (r :stdout)))`)
		case "pane_pid":
			exprs = append(exprs,
				`(string (cmd/pid p))`)
		case "I":
			// Window index; cy has no windows, always 0
			exprs = append(exprs, `"0"`)
		default:
			// Unknown variable, output empty string
			exprs = append(exprs, `""`)
		}
	}

	return "(string " + strings.Join(exprs, " ") + ")"
}

func newSessionCommand() error {
	name := CLI.NewSession.SessionName
	if name == "" {
		return fmt.Errorf("session name required")
	}

	var code string
	if len(CLI.NewSession.Command) > 0 {
		command := CLI.NewSession.Command[0]
		args := CLI.NewSession.Command[1:]

		argsJanet := "@["
		for i, arg := range args {
			if i > 0 {
				argsJanet += " "
			}
			argsJanet += quoteJanet(arg)
		}
		argsJanet += "]"

		code = fmt.Sprintf(
			`(let [g (group/new :root :name %s)]`+
				` (cmd/new g :command %s :args %s))`,
			quoteJanet(name),
			quoteJanet(command),
			argsJanet,
		)
	} else {
		code = fmt.Sprintf(
			`(let [g (group/new :root :name %s)]`+
				` (cmd/new g))`,
			quoteJanet(name),
		)
	}

	_, err := execJanet(code, true, OutputFormatRaw)
	return err
}

func hasSessionCommand() error {
	name := parseTarget(CLI.HasSession.Target)
	code := fmt.Sprintf(
		`(tree/id :root %s)`,
		quoteJanet("/"+name),
	)

	_, err := execJanet(code, false, OutputFormatRaw)
	if err != nil {
		os.Exit(1)
	}
	return nil
}

func sendKeysCommand() error {
	target := CLI.SendKeys.Target
	pane := firstPaneExpr(target)

	if CLI.SendKeys.Literal {
		text := strings.Join(CLI.SendKeys.Keys, " ")
		code := fmt.Sprintf(
			`(pane/send-text %s %s)`,
			pane,
			quoteJanet(text),
		)
		_, err := execJanet(code, false, OutputFormatRaw)
		return err
	}

	// Send as key names
	for _, key := range CLI.SendKeys.Keys {
		cyKey := tmuxKeyToCy(key)
		code := fmt.Sprintf(
			`(pane/send-keys %s @[%s])`,
			pane,
			quoteJanet(cyKey),
		)
		if _, err := execJanet(
			code,
			false,
			OutputFormatRaw,
		); err != nil {
			return err
		}
	}
	return nil
}

func capturePaneCommand() error {
	target := CLI.CapturePane.Target
	pane := firstPaneExpr(target)

	code := fmt.Sprintf(
		`(yield`+
			` (string/join`+
			` (map string/trimr (pane/screen %s))`+
			` "\n"))`,
		pane,
	)

	result, err := execJanet(code, false, OutputFormatRaw)
	if err != nil {
		return err
	}

	_, err = os.Stdout.Write(result)
	return err
}

func listPanesCommand() error {
	target := CLI.ListPanes.Target
	if target == "" {
		return fmt.Errorf("target session required")
	}

	sessionName := parseTarget(target)
	format := CLI.ListPanes.Format
	if format == "" {
		format = "#{pane_pid}"
	}

	parts := parseTmuxFormat(format)
	formatExpr := buildFormatJanet(parts)

	code := fmt.Sprintf(
		`(yield`+
			` (string/join`+
			` (seq [p :in (group/leaves`+
			` (tree/id :root %s))]`+
			` (try %s ([_] "")))`+
			` "\n"))`,
		quoteJanet("/"+sessionName),
		formatExpr,
	)

	result, err := execJanet(code, false, OutputFormatRaw)
	if err != nil {
		return err
	}

	if len(result) > 0 {
		fmt.Print(string(result))
		if !strings.HasSuffix(string(result), "\n") {
			fmt.Println()
		}
	}
	return nil
}

func killSessionCommand() error {
	name := parseTarget(CLI.KillSession.Target)
	code := fmt.Sprintf(
		`(tree/rm (tree/id :root %s))`,
		quoteJanet("/"+name),
	)

	_, err := execJanet(code, false, OutputFormatRaw)
	return err
}

func renameSessionCommand() error {
	oldName := parseTarget(CLI.RenameSession.Target)
	newName := CLI.RenameSession.NewName

	code := fmt.Sprintf(
		`(tree/set-name (tree/id :root %s) %s)`,
		quoteJanet("/"+oldName),
		quoteJanet(newName),
	)

	_, err := execJanet(code, false, OutputFormatRaw)
	return err
}

func loadBufferCommand() error {
	name := CLI.LoadBuffer.BufferName
	file := CLI.LoadBuffer.File

	data, err := os.ReadFile(file)
	if err != nil {
		return fmt.Errorf("failed to read %s: %w", file, err)
	}

	dest := bufferFilePath(name)
	return os.WriteFile(dest, data, 0600)
}

func pasteBufferCommand() error {
	name := CLI.PasteBuffer.BufferName
	target := CLI.PasteBuffer.Target
	pane := firstPaneExpr(target)

	bufPath := bufferFilePath(name)

	code := fmt.Sprintf(
		`(pane/send-text %s (slurp %s))`,
		pane,
		quoteJanet(bufPath),
	)

	_, err := execJanet(code, false, OutputFormatRaw)
	return err
}

func deleteBufferCommand() error {
	name := CLI.DeleteBuffer.BufferName
	path := bufferFilePath(name)
	// Ignore errors (buffer may not exist)
	_ = os.Remove(path)
	return nil
}

func setOptionCommand() error {
	// No-op: cy uses Janet configuration instead of tmux options
	return nil
}

func setHookCommand() error {
	// No-op: cy uses Janet for hooks
	return nil
}

func setEnvironmentCommand() error {
	// No-op: environment is managed by the pane's process
	return nil
}

func listWindowsCommand() error {
	target := CLI.ListWindows.Target
	if target == "" {
		return fmt.Errorf("target session required")
	}

	format := CLI.ListWindows.Format
	if format == "" {
		format = "#I"
	}

	// cy doesn't have windows; emit a single line for window 0
	parts := parseTmuxFormat(format)
	var result []string
	for _, part := range parts {
		if part.isVariable && part.text == "I" {
			result = append(result, "0")
		} else if !part.isVariable {
			result = append(result, part.text)
		} else {
			result = append(result, "")
		}
	}

	fmt.Println(strings.Join(result, ""))
	return nil
}

func resizeWindowCommand() error {
	// No-op: cy handles window sizing automatically
	return nil
}

func waitForCommand() error {
	// No-op: returns immediately. mngr has fallback behavior
	// for when wait-for doesn't block.
	return nil
}

func displayPopupCommand() error {
	// No-op: cy doesn't support tmux-style popups
	return nil
}

func detachClientCommand() error {
	// No-op: cy clients are managed separately
	return nil
}
