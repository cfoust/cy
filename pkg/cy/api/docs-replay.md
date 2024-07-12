# doc: Quit

Quit replay mode.

# doc: ScrollUp

Scroll the viewport one line up.

# doc: ScrollDown

Scroll the viewport one line down.

# doc: HalfPageUp

Scroll the viewport half a page (half the viewport height) up.

# doc: HalfPageDown

Scroll the viewport half a page (half the viewport height) down.

# doc: SearchForward

Search for a string forwards in time (in time mode) or in the scrollback buffer (in copy mode).

# doc: SearchBackward

Search for a string backwards in time (in time mode) or in the scrollback buffer (in copy mode).

# doc: TimePlay

Toggle playback.

# doc: SearchAgain

Go to the next match in the direction of the last search.

# doc: SearchReverse

Go to the previous match in the direction of the last search.

# doc: TimeStepBack

Step one event backward in time.

# doc: TimeStepForward

Step one event forward in time.

# doc: Beginning

Go to the beginning of the time range (in time mode) or the first line of the screen (in copy mode).

# doc: End

Go to the end of the time range (in time mode) or the last line of the screen (in copy mode).

# doc: CursorDown

Move cursor down one cell.

# doc: CursorLeft

Move cursor left one cell.

# doc: CursorRight

Move cursor right one cell.

# doc: CursorUp

Move cursor up one cell.

# doc: TimePlaybackRate

(replay/time-playback-rate rate)

Set the playback rate to `rate`. Positive numbers indicate a multiplier of real time moving _forwards_, negative numbers, _backwards_. For example, a rate of `2` means that time will advance at twice the normal speed; `-2` means that time will go backwards at -2x.

`rate` is clamped to the range [10, 10].

# doc: Copy

Yank the selection into the copy buffer.

# doc: Select

Enter visual select mode.

# doc: JumpAgain

Repeat the last character jump.

# doc: JumpReverse

Repeat the inverse of the last character jump.

# doc: JumpBackward

(replay/jump-backward char)

Jump to the previous instance of `char` on the current line.

# doc: JumpForward

(replay/jump-forward char)

Jump to the next instance of `char` on the current line.

# doc: JumpToForward

(replay/jump-to-forward char)

Jump to the cell before `char` after the cursor on the current line.

# doc: JumpToBackward

(replay/jump-to-backward char)

Jump to the cell before `char` after the cursor on the current line.

# doc: Open

(replay/open id)

Enter replay mode for pane `id` (which is a [NodeID](api.md#nodeid)).

# doc: OpenFile

(replay/open-file group path)

Open the `.borg` file found at `path` in a new replay window in `group`.

For example:

```janet
# ignore
(replay/open-file :root "some_borg.borg")
```

# doc: SwapScreen

Swap between the alt screen and the main screen. This allows you to return to the pane's scrollback without quitting a program that is using the alternate screen, such as vim or htop.

# doc: CommandForward

In time mode, jump to the moment in time just before the next command was executed. In copy mode, move the cursor to the first character of the next command.

# doc: CommandBackward

In time mode, jump to the moment in time just before the previous command was executed. In copy mode, move the cursor to the first character of the previous command that was executed.

# doc: CommandSelectForward

Move the cursor to the first character of the output of the next command and select its output.

# doc: CommandSelectBackward

Move the cursor to the first character of the output of the previous command and select its output.

# doc: StartOfLine

Move to the first character of the physical line. Equivalent to vim's `0`.

# doc: FirstNonBlank

Move to the first non-blank character of the physical line. Equivalent to vim's `^`.

# doc: EndOfLine

Move to the last character of the physical line. Equivalent to vim's `$`.

# doc: LastNonBlank

Move to the last non-blank character of the physical line. Equivalent to vim's `g_`.

# doc: StartOfScreenLine

Move to the first character of the screen line. Equivalent to vim's `g0`.

# doc: MiddleOfScreenLine

Move to the middle of the screen line. Equivalent to vim's `gm`.

# doc: MiddleOfLine

Move to the middle of the physical line. Equivalent to vim's `gM`.

# doc: EndOfScreenLine

Move to the end of the screen line. Equivalent to vim's `g$`.

# doc: FirstNonBlankScreen

Move to the first non-blank character of the screen line. Equivalent to vim's `g^`.

# doc: LastNonBlankScreen

Move to the last non-blank character of the screen line. Equivalent to vim's `g<end>`.

# doc: WordForward

Move to the beginning of the next word. Equivalent to vim's `w`.

# doc: WordBackward

Move to the beginning of the previous word. Equivalent to vim's `b`.

# doc: WordEndForward

Move to the end of the next word. Equivalent to vim's `e`.

# doc: WordEndBackward

Move to the end of the previous word. Equivalent to vim's `ge`.

# doc: BigWordForward

Move to the beginning of the next WORD. Equivalent to vim's `W`.

# doc: BigWordBackward

Move to the beginning of the previous WORD. Equivalent to vim's `B`.

# doc: BigWordEndForward

Move to the end of the next WORD. Equivalent to vim's `E`.

# doc: BigWordEndBackward

Move to the end of the previous WORD. Equivalent to vim's `gE`.
