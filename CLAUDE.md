# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`cy` is a time-traveling terminal multiplexer written in Go and Janet. It uses a server-client model where clients connect via Unix domain sockets using WebSocket connections. The main features include replaying terminal sessions, fuzzy finding, shell command history, and flexible configuration via Janet scripting.

## Build Commands

This project uses `just` as its task runner. Key commands:

```bash
# Build the main cy executable
just build

# Install to GOPATH/bin
just install

# Run all tests
just test

# Run tests with additional args (e.g., -v, specific packages)
just test -v ./pkg/mux/...

# Format code
just format

# Lint code
just lint

# Lint with auto-fixes
just lint-fix

# Generate code (go generate)
just generate

# Run cy in development mode
just run

# Serve documentation locally
just docs

# Generate API documentation
just api

# Generate sqlc code
just sqlc
```

### Running Individual Tests

```bash
# Run a specific test
go test -v ./pkg/cy -run TestSpecificTest

# Run tests in a specific package
go test ./pkg/mux/...
```

### Building

The main executable is `cmd/cy/main.go`. Build output goes to `./cy` in the repository root.

## Code Architecture

### Core Abstractions: Screen and Stream

The two most important abstractions in cy are defined in `pkg/mux/module.go`:

**Stream**: A resizable bidirectional stream of bytes (read/write/resize). Represents things like pseudo-terminals and client connections. Streams can be composed into pipes of arbitrary complexity.

**Screen**: A stateful application that can receive events (user input) and produce visual updates. Represents terminal emulator state (2D buffer of characters, cursor position/style). Examples: panes, fuzzy finder, replay mode. Screens can be composed (e.g., `Layers` renders screens on top of each other).

**Data Flow**:
1. Client input → WebSocket → Server `Client` (Stream) → Screen hierarchy
2. Screen state changes → `Renderer` → calculates byte diff → WebSocket → Client terminal

### Package Structure

**Core packages**:
- `pkg/cy`: The cy server, Janet API implementation, and default configuration
- `pkg/mux`: Screen and Stream abstractions plus implementations for compositing/rendering
- `pkg/janet`: Janet VM for Go/Janet interoperation (handles FFI complexity)
- `pkg/emu`: VT100 terminal emulator (custom implementation)
- `pkg/replay`: Terminal session player (replay mode)
- `pkg/sessions`: Recorded terminal session data types and utilities (serialize/search/export)
- `pkg/geom`: Geometric primitives (Vec2, etc.) used throughout

**User input packages**:
- `pkg/input/fuzzy`: Fuzzy finder implementation
- `pkg/input/text`: Text input field

**Other important packages**:
- `pkg/bind`: Key binding system
- `pkg/cmd`: Command tracking and storage
- `pkg/params`: Parameter system (user preferences)
- `pkg/taro`: Fork of bubbletea adapted for cy's windowing
- `pkg/stories`: Storybook-like system for UI component development
- `pkg/frames`: Frame/animation system
- `pkg/layout`: Layout composition

**Commands** (`cmd/`):
- `cy`: Main executable (client/server)
- `stories`: Visual design iteration tool
- `docs`: Dumps API/keybindings/etc as JSON for documentation generation
- `perf`: Performance testing for history search

### Janet Configuration System

The Janet API is initialized in `pkg/cy/janet.go`. Modules are registered in `initAPI()`, mapping Go types to Janet module names (e.g., `cy`, `pane`, `group`, `cmd`, etc.).

Boot files in `pkg/cy/boot/` define the default configuration and are embedded at compile time:
- `actions.janet`: Built-in actions (accessible via command palette)
- `binds.janet`: Default key bindings
- `layout.janet`: Default layout configuration
- `replay.janet`: Replay mode configuration
- Other helpers (colors, input, register, style, time)

Each API module has corresponding documentation in `pkg/cy/api/docs-*.md` and is exposed to Janet via methods on Go structs.

### Adding New Actions

When adding a new action (like `action/memory-profile`):
1. Add the Go implementation as a method on the appropriate module (e.g., `CyModule` in `pkg/cy/api.go`)
2. Register the action in `pkg/cy/boot/actions.janet` using the `key/action` macro
3. The action automatically appears in the command palette and can be bound to keys

### Client-Server Architecture

- Server daemonizes on startup, creates Unix domain socket
- Clients connect via WebSocket over Unix socket
- Each `Client` (in `pkg/cy/client.go`) is a Stream that translates between bytes and events
- Server maintains a tree of groups and panes (`pkg/cy/module.go`)
- Multiple clients can connect to the same server with different screen sizes

### Testing

Tests are colocated with implementation files (`*_test.go`). Janet tests use `.janet` extension (e.g., `pkg/cy/api/colormaps_test.janet`).

### Code Style

- Max line length: 80 characters (enforced by golines)
- Import grouping: standard library → cy packages → third-party (enforced by gci)
- The codebase uses `sasha-s/go-deadlock` for deadlock detection during development

## Stories System

The Stories system is cy's equivalent to Storybook.js - a framework for developing and testing UI components in isolation. It allows developers to create, view, and test visual components interactively.

### Core Components

- **pkg/stories/module.go** - Core stories framework with registration system
- **pkg/stories/ui/** - Browser interface for viewing and navigating stories
- **cmd/stories/main.go** - Standalone stories viewer CLI application

### How Stories Work

Stories are predefined configurations of cy's UI components that can include:
- **Visual Components** - Isolated UI elements (inputs, layouts, animations)
- **Interactive Sequences** - Automated input sequences that demonstrate functionality
- **Size Configuration** - Custom terminal dimensions for optimal viewing
- **Snapshots** - Static captures for documentation

### Story Structure

Each story consists of:
- **Name** - Hierarchical identifier (e.g., "input/fuzzy/search")
- **Init Function** - Creates and returns the component to display
- **Config** - Defines size, input sequences, and behavior

### Story Registration

Stories are registered throughout the codebase using `stories.Register()`:

```go
stories.Register("story-name", initFunc, stories.Config{
    Size: geom.Size{R: 26, C: 80}, // Custom terminal size
    Input: []interface{}{           // Automated input sequence
        stories.Type("ctrl+j"),     // Simulate key presses
        stories.Wait(stories.Some), // Add delays
        stories.Type("search text"),
    },
})
```

### Stories Across the Codebase

**Core cy Stories** (pkg/cy/stories.go):
- Layout demonstrations (splits, margins, tabs, borders)
- Replay mode functionality 
- Shell and project management
- Command palette interactions
- Theme and color map showcases

**Input Component Stories**:
- **pkg/input/fuzzy/stories.go** - Fuzzy finder in various configurations
- **pkg/input/text/stories.go** - Text input components

**UI Component Stories**:
- **pkg/mux/screen/splash/stories.go** - Splash screens
- **pkg/mux/screen/placeholder/stories.go** - Placeholder states

**Feature Stories**:
- **pkg/replay/stories.go** - Replay mode with simulated terminal sessions
- **pkg/replay/loader/stories.go** - Loading states
- **pkg/search/stories.go** - Search functionality

### Running Stories

```bash
# View all stories in an interactive browser
./cmd/stories/main.go

# Filter stories by prefix
./cmd/stories/main.go --prefix input/

# View a single story in fullscreen
./cmd/stories/main.go --single "layout/split-half"

# Export story to asciinema format
./cmd/stories/main.go --single "cy/replay" --cast output.cast
```

### Story Input System

Stories can simulate user interactions:
- **stories.Type()** - Simulate keyboard input with automatic timing
- **stories.Wait()** - Add delays (ABit, Some, More, ALot)
- **Key sequences** - Complex key combinations and navigation

### Auto-generated Stories

The system automatically creates stories for:
- **Frame patterns** (pkg/frames/) - All available terminal frames
- **Animations** (pkg/anim/) - All animation modules  
- **Themes** - Color scheme demonstrations
