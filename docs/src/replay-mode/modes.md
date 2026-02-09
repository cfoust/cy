# Modes

Like `vim`, replay mode is modal. It has several different modes that influence both what is shown on the screen and what you can do:

- **Time mode:** allows you to pause, play, and move through the history of the current pane back to when it first began.
- **Copy mode:** allows you to explore the state of the screen _at a particular point in time_. This includes the scrollback buffer, which is traditionally all that `tmux`'s copy mode gave you access to.
  - **Visual mode:** This is a submode of copy mode that permits you to select text and copy it into your user-specific copy buffer that can then be pasted elsewhere.

### Time mode

{{story cast replay/time-demo}}

Time mode is similar to a video player: you can pause, play (both forwards and backwards!), and skip through time to inspect particular moments in the history of a pane. While playing back terminal events, time mode skips inactivity, here defined as any idle period longer than a second.

#### Searching

{{story cast replay/time-demo-search}}

Time mode also allows you to search through the pane's history using regular expressions or string literals. In this way you can find all instances of a string if it ever appeared on the screen--even if it was subsequently cleared away. Note that _this is different from searching in the scrollback._ Searching in time mode will find matches that appeared on the screen at any point in time, including in full-screen applications such as `vim` or `htop`.

You can initiate a search by hitting {{bind :time /}} to search forward in time and {{bind :time ?}} to search backward (by default).

If the query string you enter is not a valid regex, it will be interpreted as a string literal.

The search bar also supports time expressions, which you can use to jump by a fixed amount of time. Time expressions are in the format `NdNhNmNs` where `N` is the number of that unit that you wish to move by. You will move in time _in the direction of your search_.

{{story cast replay/time-demo-search-time}}

Some examples:

```janet
# ignore
1h30s # one hour 30 seconds
3d # three days
```

### Copy mode

To enter copy mode, all you need to do is invoke any action that would cause the cursor or the viewport to move. Like `tmux`'s copy mode, you can explore the state of the screen and copy text to be pasted elsewhere. Copy mode supports a wide range of cursor and viewport movements that should feel familiar to users of CLI text editors such as `vim`. For a full list of supported motions, refer to the [reference page for key bindings](/default-keys.md#movements).

Copy mode also allows you to swap between the terminal's main and alt screens using {{bind :copy s}}. In other words, even if you run a full-screen application such as `htop`, you can still swap back to the scrollback buffer and see the output of commands you ran before running `htop`.

#### Visual mode

Visual mode is initiated when you press {{bind :copy v}} (by default). It works almost exactly like `vim`'s visual mode does; after you have selected some text, you can yank it into your buffer with {{bind :copy y}} and paste it elsewhere with {{bind :root ctrl+a P}}.

#### Registers

In `cy`, you can copy text to and paste text from **registers**. This system works almost identically to registers in `vim`. Each register is identified by a string key (e.g. `"a"`) and can store a string value. The bindings described in the section above ({{bind :copy y}} and {{bind :root ctrl+a P}}) copy and paste from the `""` register. The system clipboard (if available) is accessible using a special register, `"+"`.

By default, `cy` lets you copy to and paste from a range of registers that mimic those found in `vim`. In visual mode you can copy text into a register using `" [a-zA-Z0-9+] y`. For example, hitting <kbd>"</kbd> <kbd>a</kbd> <kbd>y</kbd> copies the current selection into register `"a"`. Elsewhere in `cy` you can paste from `"a"` by hitting <kbd>ctrl+a</kbd> <kbd>"</kbd> <kbd>a</kbd> <kbd>p</kbd>.

The state of registers in `cy` is **global, not per-client.** This means that after one client yanks into a register, all connected clients can paste from that register. The contents of registers are written to `cy`'s [persistent store](/parameters.md#persist) and therefore remain available even after restarting the `cy` server.

You can access these registers programmatically using the `register/*` family of API functions such as {{api register/get}} and {{api register/set}}.

For convenience, `cy` also provides {{api clipboard/get}} and {{api clipboard/set}} to quickly access the system clipboard from Janet. These functions just read and write from the special `"+"` register.
