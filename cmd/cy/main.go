package main

import (
	"fmt"
	"os"

	"github.com/cfoust/cy/pkg/cy"
	"github.com/cfoust/cy/pkg/version"

	"github.com/alecthomas/kong"
	"github.com/rs/zerolog/log"
)

var CLI struct {
	Socket string `help:"Specify the name of the socket." name:"socket-name" optional:"" short:"L" default:"default"`

	Version bool `help:"Print version information and exit." short:"v"`

	Exec struct {
		Command string `help:"Provide Janet code as a string argument." name:"command" short:"c" optional:"" default:""`
		Format  string `name:"format" optional:"" enum:"raw,json,janet" short:"f" default:"raw" help:"Set the desired output format."`
		File    string `arg:"" optional:"" help:"Provide a file containing Janet code." type:"existingfile"`
	} `cmd:"" help:"Execute Janet code on the cy server."`

	Recall struct {
		Reference string `arg:"" optional:"" help:"A reference to a command."`
	} `cmd:"" help:"Recall the output of a previous command."`

	Connect struct {
		Config string `help:"Provide a Janet configuration file to use on startup." name:"config" optional:"" default:""`
		CPU    string `help:"Save a CPU performance report to the given path." name:"perf-file" optional:"" default:""`
		Trace  string `help:"Save a trace report to the given path." name:"trace-file" optional:"" default:""`
	} `cmd:"" default:"1" aliases:"attach" help:"Connect to the cy server, starting one if necessary."`

	KillServer struct{} `cmd:"" name:"kill-server" help:"Kill the cy server."`

	Test struct {
		Files []string `arg:"" optional:"" help:"Janet test files to run." type:"existingfile"`
	} `cmd:"" help:"Run Janet test files."`

	// tmux-compatible commands for use with tools like mngr
	NewSession struct {
		Detached    bool     `short:"d" help:"Create in detached mode." default:"true"`
		SessionName string   `short:"s" help:"Session name." required:""`
		Width       int      `short:"x" help:"Window width." optional:"" default:"200"`
		Height      int      `short:"y" help:"Window height." optional:"" default:"50"`
		Command     []string `arg:"" optional:"" help:"Command to run."`
	} `cmd:"" name:"new-session" help:"Create a new session (tmux-compatible)."`

	HasSession struct {
		Target string `short:"t" help:"Target session." required:""`
	} `cmd:"" name:"has-session" help:"Check if session exists (tmux-compatible)."`

	SendKeys struct {
		Target  string   `short:"t" help:"Target pane." required:""`
		Literal bool     `short:"l" help:"Send keys literally."`
		Keys    []string `arg:"" optional:"" help:"Keys to send."`
	} `cmd:"" name:"send-keys" help:"Send keys to a pane (tmux-compatible)."`

	CapturePane struct {
		Target     string `short:"t" help:"Target pane." required:""`
		Print      bool   `short:"p" help:"Output to stdout." default:"true"`
		Scrollback string `short:"S" help:"Start line." optional:""`
	} `cmd:"" name:"capture-pane" help:"Capture pane contents (tmux-compatible)."`

	ListPanes struct {
		Target   string `short:"t" help:"Target window or session." optional:""`
		Format   string `short:"F" help:"Output format string." optional:""`
		AllPanes bool   `short:"s" help:"List all panes in session."`
	} `cmd:"" name:"list-panes" help:"List panes (tmux-compatible)."`

	KillSession struct {
		Target string `short:"t" help:"Target session." required:""`
	} `cmd:"" name:"kill-session" help:"Kill a session (tmux-compatible)."`

	RenameSession struct {
		Target  string `short:"t" help:"Target session." required:""`
		NewName string `arg:"" help:"New session name."`
	} `cmd:"" name:"rename-session" help:"Rename a session (tmux-compatible)."`

	LoadBuffer struct {
		BufferName string `short:"b" help:"Buffer name." required:""`
		File       string `arg:"" help:"File to load."`
	} `cmd:"" name:"load-buffer" help:"Load file into paste buffer (tmux-compatible)."`

	PasteBuffer struct {
		BufferName string `short:"b" help:"Buffer name." required:""`
		Target     string `short:"t" help:"Target pane." required:""`
	} `cmd:"" name:"paste-buffer" help:"Paste buffer to pane (tmux-compatible)."`

	DeleteBuffer struct {
		BufferName string `short:"b" help:"Buffer name." required:""`
	} `cmd:"" name:"delete-buffer" help:"Delete a paste buffer (tmux-compatible)."`

	SetOption struct {
		Target string   `short:"t" help:"Target session." optional:""`
		Args   []string `arg:"" optional:""`
	} `cmd:"" name:"set-option" help:"Set option (tmux-compatible, no-op)."`

	SetHook struct {
		Target string   `short:"t" help:"Target session." optional:""`
		Args   []string `arg:"" optional:""`
	} `cmd:"" name:"set-hook" help:"Set hook (tmux-compatible, no-op)."`

	SetEnvironment struct {
		Target string   `short:"t" help:"Target session." optional:""`
		Args   []string `arg:"" optional:""`
	} `cmd:"" name:"set-environment" help:"Set environment variable (tmux-compatible, no-op)."`

	ListWindows struct {
		Target string `short:"t" help:"Target session." required:""`
		Format string `short:"F" help:"Output format string." optional:""`
	} `cmd:"" name:"list-windows" help:"List windows (tmux-compatible)."`

	ResizeWindow struct {
		Target    string `short:"t" help:"Target window." required:""`
		Automatic bool   `short:"A" help:"Resize to fit."`
	} `cmd:"" name:"resize-window" help:"Resize window (tmux-compatible, no-op)."`

	WaitFor struct {
		Channel string `arg:"" help:"Channel name."`
	} `cmd:"" name:"wait-for" help:"Wait for channel (tmux-compatible, no-op)."`

	DisplayPopup struct {
		Args []string `arg:"" optional:""`
	} `cmd:"" name:"display-popup" help:"Display popup (tmux-compatible, no-op)."`

	DetachClient struct {
		Target string `short:"t" help:"Target session." optional:""`
	} `cmd:"" name:"detach-client" help:"Detach client (tmux-compatible, no-op)."`
}

func writeError(err error) {
	fmt.Fprintf(os.Stderr, "%s\n", err)
	os.Exit(1)
}

func main() {
	// Shortcut for getting output e.g. cy -1
	if len(os.Args) == 2 {
		arg := os.Args[1]
		if _, err := parseReference(arg); err == nil {
			CLI.Socket = "default"
			err := recallCommand(arg)
			if err != nil {
				writeError(err)
			}
			return
		}
	}

	// This seems crazy, but the negative numbers passed to `cy recall` are
	// interpreted as flags by kong (it does not appear we can configure
	// this,) so we need to adjust them before parsing.
	if len(os.Args) > 1 && os.Args[1] == "recall" {
		for i, arg := range os.Args {
			if RELATIVE_REFERENCE.MatchString(arg) {
				os.Args[i] = "!" + arg
			}
		}
	}

	ctx := kong.Parse(&CLI,
		kong.Name("cy"),
		kong.Description("the time traveling terminal multiplexer"),
		kong.UsageOnError(),
		kong.ConfigureHelp(kong.HelpOptions{
			Compact: true,
			Summary: true,
		}))

	if CLI.Version {
		fmt.Printf(
			"cy %s (commit %s)\n",
			version.Version,
			version.GitCommit,
		)
		fmt.Printf(
			"built %s\n",
			version.BuildTime,
		)
		os.Exit(0)
	}

	switch ctx.Command() {
	case "test", "test <files>":
		err := testCommand()
		if err != nil {
			writeError(err)
		}
		return
	}

	if !cy.SOCKET_REGEX.MatchString(CLI.Socket) {
		log.Fatal().
			Msg("invalid socket name, the socket name must be alphanumeric")
	}

	var err error
	switch ctx.Command() {
	case "exec", "exec <file>":
		err = execCommand()
	case "recall <reference>":
		ref := CLI.Recall.Reference
		if ref[0] == '!' {
			ref = ref[1:]
		}
		err = recallCommand(ref)
	case "connect":
		err = connectCommand()
	case "kill-server":
		err = killServerCommand()
	// tmux-compatible commands
	case "new-session", "new-session <command>":
		err = newSessionCommand()
	case "has-session":
		err = hasSessionCommand()
	case "send-keys", "send-keys <keys>":
		err = sendKeysCommand()
	case "capture-pane":
		err = capturePaneCommand()
	case "list-panes":
		err = listPanesCommand()
	case "kill-session":
		err = killSessionCommand()
	case "rename-session", "rename-session <new-name>":
		err = renameSessionCommand()
	case "load-buffer", "load-buffer <file>":
		err = loadBufferCommand()
	case "paste-buffer":
		err = pasteBufferCommand()
	case "delete-buffer":
		err = deleteBufferCommand()
	case "set-option", "set-option <args>":
		err = setOptionCommand()
	case "set-hook", "set-hook <args>":
		err = setHookCommand()
	case "set-environment", "set-environment <args>":
		err = setEnvironmentCommand()
	case "list-windows":
		err = listWindowsCommand()
	case "resize-window":
		err = resizeWindowCommand()
	case "wait-for", "wait-for <channel>":
		err = waitForCommand()
	case "display-popup", "display-popup <args>":
		err = displayPopupCommand()
	case "detach-client":
		err = detachClientCommand()
	}

	if err != nil {
		writeError(err)
	}
}
