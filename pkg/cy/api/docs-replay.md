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

Search for a string forwards in time.

# doc: SearchBackward

Search for a string backwards in time.

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

(replay/open group path)

Open the `.borg` file found at `path` in a new replay window in `group`.

For example:

```janet
(replay/open (tree/root) "some_borg.borg")
```
