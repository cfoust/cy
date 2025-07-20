# CLAUDE.md

This is a comprehensive guide for Claude Code to understand and work with the `cy` codebase.

## Project Overview

`cy` is an experimental terminal multiplexer written in Go that aims to be a simple, modern, and ergonomic alternative to `tmux`. It features time-traveling capabilities, allowing users to replay and search through terminal sessions.

**Key Features:**
- Replay and search through any terminal session
- Shell command history with revisitation capabilities
- Flexible configuration using Janet programming language
- Built-in fuzzy finding using concepts from fzf
- Modern, ergonomic design

## Architecture

This is a Go project with a modular architecture organized into several key packages:

### Core Components
- **cmd/cy/** - Main CLI application entry point
- **pkg/cy/** - Core cy functionality and API
- **pkg/mux/** - Terminal multiplexer implementation
- **pkg/replay/** - Time-traveling and replay functionality
- **pkg/emu/** - Terminal emulator implementation
- **pkg/layout/** - UI layout system
- **pkg/input/** - Input handling and fuzzy finding
- **pkg/sessions/** - Session management
- **pkg/search/** - Search functionality
- **pkg/style/** - Styling and theming system

### Notable Features
- **Janet Integration** (pkg/janet/) - Embedded Janet language for configuration
- **WebSocket Support** (pkg/io/ws/) - For remote connections
- **Animation System** (pkg/anim/) - For terminal animations
- **Color Maps** (pkg/style/colormaps/) - Extensive theming support
- **Stories System** (pkg/stories/) - Interactive visual testing and documentation (like Storybook.js)

## Build System

The project uses:
- **Go modules** for dependency management
- **justfile** for build automation (similar to Makefile)
- **mdbook** for documentation

### Key Commands

```bash
# Build the project
just build

# Install cy
just install

# Run tests
just test
go test ./pkg/... ./cmd/...

# Run Janet API tests
go test ./pkg/cy/ -run TestAPI

# Format code
just format
go fmt ./pkg/... ./cmd/...

# Run in development mode
just run

# Serve documentation
just serve-docs
```

## Development Workflow

1. **Building**: Use `just build` or `go build -o ./cy ./cmd/cy/...`
2. **Testing**: Use `just test` to run the full test suite
3. **Running**: Use `just run` to run cy in development mode with logging
4. **Documentation**: The project has extensive documentation in `docs/` built with mdbook

## Key Files and Directories

- **cmd/cy/main.go** - Application entry point
- **pkg/cy/api.go** - Core API implementation
- **pkg/mux/screen/** - Screen management and terminal handling
- **pkg/replay/** - Time-traveling terminal replay system
- **pkg/janet/** - Janet language integration for configuration
- **docs/** - Comprehensive documentation source
- **justfile** - Build automation recipes

## Configuration

cy uses Janet (a Lisp-like language) for configuration. The configuration system is implemented in:
- **pkg/cy/config.go** - Configuration loading
- **pkg/cy/boot/** - Default configuration files
- **pkg/janet/** - Janet language integration

## Testing

- Test files are located throughout the codebase using Go's standard `*_test.go` convention
- Janet tests use `*_test.janet` files
- Use `just test` or `go test ./pkg/... ./cmd/...` to run tests

## Documentation

The project has extensive documentation:
- Source files in `docs/src/`
- Built documentation in `docs/book/`
- Live documentation at https://cfoust.github.io/cy/
- Use `just serve-docs` to serve docs locally

## Code Style

- Follow standard Go formatting (`go fmt`)
- Use `just format` to format the entire codebase
- The project follows Go best practices and conventions

## Dependencies

Key external dependencies include:
- **bubbletea/lipgloss** - TUI framework and styling
- **janet-lang** - Configuration language (embedded C)
- **websocket** - WebSocket support
- **sqlite3** - Database functionality
- **pty** - Pseudoterminal handling

## Entry Points

- **cmd/cy/main.go** - Main CLI application
- **cmd/docs/main.go** - Documentation generation
- **cmd/example/main.go** - Example/demo code
- **cmd/perf/main.go** - Performance testing tools
- **cmd/stories/main.go** - Stories browser and viewer for visual testing

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
