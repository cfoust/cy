# tmux.janet - tmux default keybinding compatibility for cy
#
# This file implements the default tmux keybindings using cy's API.
# The tmux prefix key C-b is mapped to cy's "ctrl+b" in this implementation.
#
# Based on tmux's default key bindings from the manual page.

# Define tmux prefix key (C-b in tmux)
(def- tmux-prefix "ctrl+b")

# === TMUX COMPATIBILITY ACTIONS ===
# These actions implement tmux-specific behaviors using exact tmux command names

(key/action
  tmux/send-prefix
  "Send the prefix key (C-b) through to the application."
  (pane/send-keys (pane/current) @["ctrl+b"]))

(key/action
  tmux/rotate-window
  "Rotate the panes in the current window forwards."
  # cy doesn't have direct window rotation, use pane navigation
  (tmux/next-window))

(key/action
  tmux/suspend-client
  "Suspend the cy client (tmux suspend-client equivalent)."
  # In cy, we detach rather than suspend since there's no direct equivalent
  (if (input/ok? "detach from the cy server?")
    (cy/detach)))

(key/action
  tmux/next-layout
  "Arrange the current window in the next preset layout."
  (msg/toast :info "Layout cycling not implemented. Use splits manually."))

(key/action
  tmux/break-pane
  "Break the current pane out of the window."
  # In cy, we can move a pane to the root level
  (def current-pane (pane/current))
  (def shells (group/mkdir :root "/shells"))
  (if (not (nil? current-pane))
    (do
      (tree/mv current-pane shells)
      (pane/attach current-pane))))

(key/action
  tmux/split-window
  "Split the current pane into two, top and bottom."
  (def [ok path] (protect (cmd/path (pane/current))))
  (def shells (group/mkdir :root "/shells"))
  (def shell (cmd/new shells
                      :path (if ok path nil)
                      :name (if ok (path/base path) "shell")))
  (layout/set
    (layout/split-down
      (layout/get)
      {:type :pane :id shell :attached true})))

(key/action
  tmux/split-window-h
  "Split the current pane into two, left and right."
  (def [ok path] (protect (cmd/path (pane/current))))
  (def shells (group/mkdir :root "/shells"))
  (def shell (cmd/new shells
                      :path (if ok path nil)
                      :name (if ok (path/base path) "shell")))
  (layout/set
    (layout/split-right
      (layout/get)
      {:type :pane :id shell :attached true})))

(key/action
  tmux/list-buffers
  "List all paste buffers."
  # cy uses the system clipboard primarily
  (msg/toast :info "cy uses system clipboard. Use action/paste to paste."))

(key/action
  tmux/rename-session
  "Rename the current session."
  # In cy, we don't have sessions exactly like tmux, but we can rename the root group
  (msg/toast :info "cy doesn't have sessions like tmux. Use tree/set-name for nodes."))

(key/action
  tmux/kill-window
  "Kill the current window."
  # In cy, this is equivalent to killing the layout pane
  (if (input/ok? "kill the current pane and remove from layout?")
    (do
      (def layout (layout/get))
      (def {:id id} (layout/path layout (layout/attach-path layout)))
      (layout/set (layout/remove-attached layout))
      (if (not (nil? id)) (tree/rm id)))))

(key/action
  tmux/select-window
  "Prompt for a window index to select."
  # In cy, we jump to panes instead
  (action/jump-pane))

(key/action
  tmux/switch-client-p
  "Switch to the previous session."
  # cy doesn't have sessions like tmux
  (msg/toast :info "cy doesn't have sessions like tmux."))

(key/action
  tmux/switch-client-n
  "Switch to the next session."
  # cy doesn't have sessions like tmux
  (msg/toast :info "cy doesn't have sessions like tmux."))

(key/action
  tmux/rename-window
  "Rename the current window."
  # In cy, we can rename the current pane/tab
  (action/rename-pane))

(key/action
  tmux/delete-buffer
  "Delete the most recently copied buffer of text."
  # cy uses system clipboard
  (msg/toast :info "cy uses system clipboard. Clear clipboard externally if needed."))

(key/action
  tmux/move-window
  "Prompt for an index to move the current window."
  # In cy, this doesn't have a direct equivalent
  (msg/toast :info "Window moving not directly supported. Use layout functions."))

(key/action
  tmux/choose-buffer
  "Choose which buffer to paste interactively from a list."
  # cy primarily uses system clipboard
  (action/paste))

(key/action
  tmux/list-keys
  "List all key bindings."
  (tmux/command-prompt))

(key/action
  tmux/choose-client
  "Choose a client to detach."
  # cy doesn't have multiple clients like tmux
  (if (input/ok? "detach from cy?")
    (tmux/detach-client)))

(key/action
  tmux/switch-client-l
  "Switch back to the last session."
  # cy doesn't have sessions
  (msg/toast :info "cy doesn't have sessions like tmux."))

(key/action
  tmux/paste-buffer
  "Paste the most recently copied buffer of text."
  (action/paste))

(key/action
  tmux/find-window
  "Prompt to search for text in open windows."
  (action/jump-screen-lines))

(key/action
  tmux/display-message
  "Display some information about the current window."
  (msg/toast :info (string "Current pane: " (tree/path (pane/current)))))

(key/action
  tmux/last-window
  "Move to the previously selected window."
  (tmux/last-pane))

(key/action
  tmux/next-window
  "Change to the next window."
  (tmux/next-window))

(key/action
  tmux/select-pane
  "Select the next pane in the current window."
  (tmux/next-window))

(key/action
  tmux/previous-window
  "Change to the previous window."
  (tmux/last-pane))

(key/action
  tmux/display-panes
  "Briefly display pane indexes."
  (action/jump-pane))

(key/action
  tmux/refresh-client
  "Force redraw of the attached client."
  # cy handles this automatically, but we can show a message
  (msg/toast :info "Screen refresh not needed in cy."))

(key/action
  tmux/mark-pane
  "Mark the current pane."
  # cy doesn't have pane marking
  (msg/toast :info "Pane marking not implemented in cy."))

(key/action
  tmux/clear-mark
  "Clear the marked pane."
  (msg/toast :info "Pane marking not implemented in cy."))

(key/action
  tmux/choose-tree
  "Select a new session interactively."
  # In cy, we can jump between different parts of the tree
  (action/jump-pane))

(key/action
  tmux/clock-mode
  "Show the time."
  (def current-time (os/date))
  (msg/toast :info (string/format "%04d-%02d-%02d %02d:%02d:%02d"
                                  (current-time :year)
                                  (+ 1 (current-time :month))
                                  (current-time :month-day)
                                  (current-time :hours)
                                  (current-time :minutes)
                                  (current-time :seconds))))

(key/action
  tmux/choose-window
  "Choose the current window interactively."
  (action/jump-pane))

(key/action
  tmux/kill-pane
  "Kill the current pane."
  (if (input/ok? "kill the current pane?")
    (action/kill-layout-pane)))

(key/action
  tmux/resize-pane
  "Toggle zoom state of the current pane."
  # cy doesn't have zoom, but we can toggle margins
  (action/toggle-margins))

(key/action
  tmux/swap-pane
  "Swap the current pane with the previous pane."
  # cy doesn't have direct pane swapping
  (msg/toast :info "Pane swapping not directly supported in cy."))

(key/action
  tmux/swap-pane-d
  "Swap the current pane with the next pane."
  # cy doesn't have direct pane swapping
  (msg/toast :info "Pane swapping not directly supported in cy."))

(key/action
  tmux/show-messages
  "Show previous messages from tmux, if any."
  # cy doesn't have a message history like tmux
  (msg/toast :info "Message history not available in cy."))

(key/action
  tmux/copy-mode
  "Enter copy mode and scroll one page up."
  (replay/open (pane/current)))

# Layout arrangement actions (M-1 through M-5)
(key/action
  tmux/select-layout
  "Arrange panes in even-horizontal layout."
  (msg/toast :info "Layout presets not implemented. Use splits manually."))

(key/action
  tmux/select-layout
  "Arrange panes in even-vertical layout."
  (msg/toast :info "Layout presets not implemented. Use splits manually."))

(key/action
  tmux/select-layout
  "Arrange panes in main-horizontal layout."
  (msg/toast :info "Layout presets not implemented. Use splits manually."))

(key/action
  tmux/select-layout
  "Arrange panes in main-vertical layout."
  (msg/toast :info "Layout presets not implemented. Use splits manually."))

(key/action
  tmux/select-layout
  "Arrange panes in tiled layout."
  (msg/toast :info "Layout presets not implemented. Use splits manually."))

(key/action
  tmux/next-layout
  "Arrange the current window in the next preset layout."
  (msg/toast :info "Layout cycling not implemented. Use splits manually."))

(key/action
  tmux/rotate-backward
  "Rotate the panes in the current window backwards."
  (msg/toast :info "Pane rotation not implemented in cy."))

# Additional tmux command actions
(key/action
  tmux/new-window
  "Create a new window."
  (action/new-shell))

(key/action
  tmux/detach-client
  "Detach the current client."
  (tmux/detach-client))

(key/action
  tmux/last-pane
  "Move to the previously active pane."
  (tmux/last-pane))

(key/action
  tmux/command-prompt
  "Enter the tmux command prompt."
  (tmux/command-prompt))

# === TMUX KEY BINDINGS ===
# Map tmux's default key bindings to cy actions

(key/bind-many-tag :root "tmux-general"
                   # Send prefix key through
                   [tmux-prefix "ctrl+b"] tmux/send-prefix

                   # Rotate panes forward (C-o)
                   [tmux-prefix "ctrl+o"] pane/history-forward

                   # Suspend client (C-z)
                   [tmux-prefix "ctrl+z"] tmux/suspend-client

                   # Break pane (!)
                   [tmux-prefix "!"] tmux/break-pane

                   # Split horizontal (")
                   [tmux-prefix "\""] tmux/split-window

                   # List paste buffers (#)
                   [tmux-prefix "#"] tmux/list-buffers

                   # Rename session ($)
                   [tmux-prefix "$"] tmux/rename-session

                   # Split vertical (%)
                   [tmux-prefix "%"] tmux/split-window-h

                   # Kill window (&)
                   [tmux-prefix "&"] tmux/kill-window

                   # Prompt for window index (')
                   [tmux-prefix "'"] tmux/select-window

                   # Previous session (()
                   [tmux-prefix "("] tmux/switch-client-p

                   # Next session ())
                   [tmux-prefix ")"] tmux/switch-client-n

                   # Rename window (,)
                   [tmux-prefix ","] tmux/rename-window

                   # Delete buffer (-)
                   [tmux-prefix "-"] tmux/delete-buffer

                   # Move window (.)
                   [tmux-prefix "."] tmux/move-window

                   # Window 0-9
                   [tmux-prefix "0"] action/jump-pane
                   [tmux-prefix "1"] action/jump-pane
                   [tmux-prefix "2"] action/jump-pane
                   [tmux-prefix "3"] action/jump-pane
                   [tmux-prefix "4"] action/jump-pane
                   [tmux-prefix "5"] action/jump-pane
                   [tmux-prefix "6"] action/jump-pane
                   [tmux-prefix "7"] action/jump-pane
                   [tmux-prefix "8"] action/jump-pane
                   [tmux-prefix "9"] action/jump-pane

                   # Command prompt (:)
                   [tmux-prefix ":"] tmux/command-prompt

                   # Last pane (;)
                   [tmux-prefix ";"] tmux/last-pane

                   # Choose buffer (=)
                   [tmux-prefix "="] tmux/choose-buffer

                   # List keys (?)
                   [tmux-prefix "?"] tmux/list-keys

                   # Choose client (D)
                   [tmux-prefix "D"] tmux/choose-client

                   # Last session (L)
                   [tmux-prefix "L"] tmux/switch-client-l

                   # Copy mode ([)
                   [tmux-prefix "["] tmux/copy-mode

                   # Paste buffer (])
                   [tmux-prefix "]"] tmux/paste-buffer

                   # Create window (c)
                   [tmux-prefix "c"] tmux/new-window

                   # Detach (d)
                   [tmux-prefix "d"] tmux/detach-client

                   # Find window (f)
                   [tmux-prefix "f"] tmux/find-window

                   # Display info (i)
                   [tmux-prefix "i"] tmux/display-message

                   # Last window (l)
                   [tmux-prefix "l"] tmux/last-window

                   # Next window (n)
                   [tmux-prefix "n"] tmux/next-window

                   # Select pane (o)
                   [tmux-prefix "o"] tmux/select-pane

                   # Previous window (p)
                   [tmux-prefix "p"] tmux/previous-window

                   # Display panes (q)
                   [tmux-prefix "q"] tmux/display-panes

                   # Refresh client (r)
                   [tmux-prefix "r"] tmux/refresh-client

                   # Mark pane (m)
                   [tmux-prefix "m"] tmux/mark-pane

                   # Clear mark (M)
                   [tmux-prefix "M"] tmux/clear-mark

                   # Choose tree (s)
                   [tmux-prefix "s"] tmux/choose-tree

                   # Clock mode (t)
                   [tmux-prefix "t"] tmux/clock-mode

                   # Choose window (w)
                   [tmux-prefix "w"] tmux/choose-window

                   # Kill pane (x)
                   [tmux-prefix "x"] tmux/kill-pane

                   # Zoom pane (z)
                   [tmux-prefix "z"] tmux/resize-pane

                   # Swap pane up ({)
                   [tmux-prefix "{"] tmux/swap-pane

                   # Swap pane down (})
                   [tmux-prefix "}"] tmux/swap-pane-d

                   # Show messages (~)
                   [tmux-prefix "~"] tmux/show-messages

                   # Copy mode page up (Page Up)
                   [tmux-prefix "pageup"] tmux/copy-mode)

# Pane movement (arrow keys)
(key/bind-many-tag :root "tmux-movement"
                   [tmux-prefix "up"] action/move-up
                   [tmux-prefix "down"] action/move-down
                   [tmux-prefix "left"] action/move-left
                   [tmux-prefix "right"] action/move-right)

# Layout arrangement (M-1 to M-5)
(key/bind-many-tag :root "tmux-layouts"
                   [tmux-prefix "alt+1"] tmux/select-layout
                   [tmux-prefix "alt+2"] tmux/select-layout
                   [tmux-prefix "alt+3"] tmux/select-layout
                   [tmux-prefix "alt+4"] tmux/select-layout
                   [tmux-prefix "alt+5"] tmux/select-layout
                   [tmux-prefix " "] tmux/next-layout
                   [tmux-prefix "alt+n"] tmux/next-window
                   [tmux-prefix "alt+o"] tmux/rotate-backward
                   [tmux-prefix "alt+p"] tmux/last-pane)

# Pane resizing (C-arrow keys for 1 cell, M-arrow keys for 5 cells)
(key/bind-many-tag :root "tmux-resize"
                   # These don't have direct cy equivalents, but we can bind them to movement
                   [tmux-prefix "ctrl+up"] action/move-up
                   [tmux-prefix "ctrl+down"] action/move-down
                   [tmux-prefix "ctrl+left"] action/move-left
                   [tmux-prefix "ctrl+right"] action/move-right
                   [tmux-prefix "alt+up"] action/move-up
                   [tmux-prefix "alt+down"] action/move-down
                   [tmux-prefix "alt+left"] action/move-left
                   [tmux-prefix "alt+right"] action/move-right)

(merge-module root-env (curenv))
