(def- prefix "ctrl+a")

(defn
  param/rset
  ```Set the value of a parameter at `:root`.```
  [key value]
  (param/set :root key value))

(key/bind-many-tag :root "general"
                   [prefix "ctrl+p"] action/command-palette
                   [prefix "q"] action/kill-server
                   [prefix "d"] action/detach
                   [prefix "F"] action/choose-frame
                   [prefix "p"] action/open-replay
                   [prefix "r"] action/reload-config
                   [prefix "P"] cy/paste)

(key/bind-many-tag :root "panes"
                   [prefix "ctrl+i"] pane/history-forward
                   [prefix "ctrl+o"] pane/history-backward
                   [prefix "x"] action/remove-current-pane
                   [prefix "X"] action/kill-current-pane
                   [prefix "C"] action/jump-command
                   [prefix ":"] action/jump-screen-lines
                   [prefix "j"] action/new-shell
                   [prefix "n"] action/new-project
                   [prefix "k"] action/jump-project
                   [prefix "l"] action/jump-shell
                   [prefix ";"] action/jump-pane
                   [prefix "c"] action/jump-pane-command
                   [prefix "|"] action/split-right
                   [prefix "-"] action/split-down
                   [prefix "H"] action/move-left
                   [prefix "L"] action/move-right
                   [prefix "K"] action/move-up
                   [prefix "J"] action/move-down
                   [prefix "left"] action/move-left
                   [prefix "right"] action/move-right
                   [prefix "up"] action/move-up
                   [prefix "down"] action/move-down)

(key/bind-many-tag :root "viewport"
                   [prefix "g"] action/toggle-margins
                   [prefix "1"] action/margins-80
                   [prefix "2"] action/margins-160)

(key/bind-many-tag :root "unprefixed"
                   ["ctrl+l"] action/next-pane)

(key/action
  action/replay-playback-1x
  "Set the playback rate to 1x real time."
  (replay/time-playback-rate 1))

(key/action
  action/replay-playback-2x
  "Set the playback rate to 2x real time."
  (replay/time-playback-rate 2))

(key/action
  action/replay-playback-5x
  "Set the playback rate to 5x real time."
  (replay/time-playback-rate 5))

(key/action
  action/replay-playback-reverse-1x
  "Set the playback rate to -1x real time (backwards)."
  (replay/time-playback-rate -1))

(key/action
  action/replay-playback-reverse-2x
  "Set the playback rate to -2x real time (backwards)."
  (replay/time-playback-rate -2))

(key/action
  action/replay-playback-reverse-5x
  "Set the playback rate to -5x real time (backwards)."
  (replay/time-playback-rate -5))

(key/bind-many-tag :time "general"
                   ["q"] replay/quit
                   ["ctrl+c"] replay/quit
                   ["esc"] replay/quit
                   ["]" "c"] replay/command-forward
                   ["[" "c"] replay/command-backward
                   ["right"] replay/time-step-forward
                   ["left"] replay/time-step-back
                   ["/"] replay/search-forward
                   ["?"] replay/search-backward
                   ["g" "g"] replay/beginning
                   ["n"] replay/search-again
                   ["N"] replay/search-reverse
                   [" "] replay/time-play
                   ["1"] action/replay-playback-1x
                   ["2"] action/replay-playback-2x
                   ["3"] action/replay-playback-5x
                   ["!"] action/replay-playback-reverse-1x
                   ["@"] action/replay-playback-reverse-2x
                   ["#"] action/replay-playback-reverse-5x
                   ["G"] replay/end)

(key/bind-many-tag :copy "general"
                   ["v"] replay/select
                   ["y"] replay/copy)

(key/bind-many-tag :copy "motion"
                   ["g" "g"] replay/beginning
                   ["G"] replay/end
                   ["/"] replay/search-forward
                   ["?"] replay/search-backward
                   ["q"] replay/quit
                   ["ctrl+c"] replay/quit
                   ["esc"] replay/quit
                   ["left"] replay/cursor-left
                   ["l"] replay/cursor-right
                   # ??? <BS> in vim actually goes across lines
                   ["backspace"] replay/cursor-left
                   ["right"] replay/cursor-right
                   # ??? <space> in vim actually goes across lines
                   [" "] replay/cursor-right
                   ["h"] replay/cursor-left
                   ["ctrl+h"] replay/cursor-left
                   ["ctrl+u"] replay/half-page-up
                   ["ctrl+d"] replay/half-page-down
                   ["up"] replay/scroll-up
                   ["down"] replay/scroll-down
                   ["j"] replay/cursor-down
                   ["k"] replay/cursor-up
                   ["n"] replay/search-again
                   ["N"] replay/search-reverse
                   ["s"] replay/swap-screen
                   ["w"] replay/word-forward
                   ["b"] replay/word-backward
                   ["e"] replay/word-end-forward
                   ["g" "e"] replay/word-end-backward
                   ["W"] replay/big-word-forward
                   ["B"] replay/big-word-backward
                   ["E"] replay/big-word-end-forward
                   ["g" "E"] replay/big-word-end-backward
                   ["]" "c"] replay/command-forward
                   ["[" "c"] replay/command-backward
                   ["]" "C"] replay/command-select-forward
                   ["[" "C"] replay/command-select-backward
                   ["0"] replay/start-of-line
                   ["home"] replay/start-of-line
                   ["g" "M"] replay/middle-of-line
                   ["$"] replay/end-of-line
                   ["^"] replay/first-non-blank
                   ["g" "_"] replay/last-non-blank
                   ["g" "0"] replay/start-of-screen-line
                   ["g" "home"] replay/start-of-screen-line
                   ["g" "m"] replay/middle-of-screen-line
                   ["g" "$"] replay/end-of-screen-line
                   ["g" "^"] replay/first-non-blank-screen
                   ["g" "end"] replay/last-non-blank-screen
                   [";"] replay/jump-again
                   [","] replay/jump-reverse
                   ["f" [:re "."]] replay/jump-forward
                   ["F" [:re "."]] replay/jump-backward
                   ["t" [:re "."]] replay/jump-to-forward
                   ["T" [:re "."]] replay/jump-to-backward)
