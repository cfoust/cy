(def prefix "ctrl+a")

(def projects (group/new (tree/root) :name "projects"))
(def shells (group/new (tree/root) :name "shells"))

(def- actions @[])

(defmacro key/def
  "register an action"
  [name docstring & body]
  ~(upscope
     (def ,name (fn ,name [&] ,;body))
     (,array/push actions [,docstring ,name])))

(key/def
  cy/command-palette
  "open command palette"
  (as?-> actions _
         (input/find _ :animated false :prompt "search: actions")
         (apply _)))

(key/def
  ot/new-shell
  "oakthree: create a new shell"
  (def path (cmd/path (pane/current)))
  (def shell (cmd/new shells path :name (path/base path)))
  (pane/attach shell))

(key/def
  ot/new-project
  "oakthree: create a new project"
  (def path (cmd/path (pane/current)))
  (def project (group/new projects :name (path/base path)))
  (def editor
    (cmd/new project
             path
             :name "editor"
             :command (os/getenv "EDITOR" "vim")))
  (def shell (cmd/new project path :name "shell"))
  (pane/attach editor))

(key/def
  ot/jump-project
  "oakthree: jump to a project"
  (as?-> projects _
         (group/children _)
         (map |(tuple (tree/name $) [:node [((group/children $) 0)]] $) _)
         (input/find _ :prompt "search: project")
         (group/children _)
         (_ 0) # Gets the first index, the editor
         (pane/attach _)))

(key/def
  ot/jump-shell
  "oakthree: jump to a shell"
  (as?-> (group/children shells) _
         (map |(tuple (cmd/path $) [:node [$]] $) _)
         (input/find _ :prompt "search: shell")
         (pane/attach _)))

(key/def
  ot/next-pane
  "oakthree: move to the next pane"
  (def children
    (-?>>
      (pane/current)
      (tree/parent)
      (group/children)
      (filter tree/pane?)))

  (when (nil? children) (break))

  (def index (index-of (pane/current) children))

  (def next-panes
    (array/concat
      (array)
      (array/slice children (+ index 1))
      (array/slice children 0 index)))

  (when (= 0 (length next-panes)) (break))
  (def [next] next-panes)
  (pane/attach next))

(key/def
  cy/jump-pane
  "jump to a pane"
  (as?-> (group/leaves (tree/root)) _
         (map |(tuple (tree/path $) [:node [$]] $) _)
         (input/find _ :prompt "search: pane")
         (pane/attach _)))

(key/def
  cy/kill-current-pane
  "kill the current pane"
  (tree/kill (pane/current)))

(key/def
  cy/toggle-margins
  "toggle margins"
  (def size (frame/size))
  (case (+ (size 0) (size 1))
    0 (frame/set-size [0 80])
    (frame/set-size [0 0])))

(key/def
  cy/margins-80
  "set size to 80 columns"
  (frame/set-size [0 80]))

(key/def
  cy/margins-160
  "set size to 160 columns"
  (frame/set-size [0 160]))

(key/def
  cy/margins-smaller
  "decrease margins by 5 columns"
  (def [lines cols] (frame/size))
  (frame/set-size [lines (+ cols 10)]))

(key/def
  cy/margins-bigger
  "increase margins by 5 columns"
  (def [lines cols] (frame/size))
  (frame/set-size [lines (- cols 10)]))

(key/def
  cy/open-log
  "open an existing log file"
  (as?-> (path/glob (path/join [(cy/get :data-dir) "*.borg"])) _
         (map |(tuple $ [:replay [$]] $) _)
         (input/find _ :prompt "search: log file")
         (replay/open (tree/root) _)
         (pane/attach _)))

(key/bind :root [prefix "j"] ot/new-shell)
(key/bind :root [prefix "n"] ot/new-project)
(key/bind :root [prefix "k"] ot/jump-project)
(key/bind :root [prefix "l"] ot/jump-shell)
(key/bind :root ["ctrl+l"] ot/next-pane)

(key/bind :root [prefix ";"] cy/jump-pane)
(key/bind :root [prefix "ctrl+p"] cy/command-palette)
(key/bind :root [prefix "x"] cy/kill-current-pane)
(key/bind :root [prefix "g"] cy/toggle-margins)
(key/bind :root [prefix "1"] cy/margins-80)
(key/bind :root [prefix "2"] cy/margins-160)
(key/bind :root [prefix "+"] cy/margins-smaller)
(key/bind :root [prefix "-"] cy/margins-bigger)
#(key/bind :root [prefix "q"] "kill the cy server" (fn [&] (cy/kill-server)))
#(key/bind :root [prefix "d"] "detach from the cy server" (fn [&] (cy/detach)))
#(key/bind :root [prefix "p"] "enter replay mode" (fn [&] (cy/replay)))
#(key/bind :root [prefix "P"] "" (fn [&] (cy/paste)))
(key/bind :root [prefix "q"] cy/kill-server)
(key/bind :root [prefix "d"] cy/detach)
(key/bind :root [prefix "p"] cy/replay)
(key/bind :root [prefix "P"] cy/paste)

(key/bind :replay ["q"] replay/quit)
(key/bind :replay ["ctrl+c"] replay/quit)
(key/bind :replay ["esc"] replay/quit)
(key/bind :replay ["right"] replay/time-step-forward)
(key/bind :replay ["left"] replay/time-step-back)
(key/bind :replay ["up"] replay/scroll-up)
(key/bind :replay ["down"] replay/scroll-down)
(key/bind :replay ["ctrl+u"] replay/half-page-up)
(key/bind :replay ["ctrl+d"] replay/half-page-down)
(key/bind :replay ["/"] replay/search-forward)
(key/bind :replay ["?"] replay/search-backward)
(key/bind :replay ["g" "g"] replay/beginning)
(key/bind :replay ["G"] replay/end)
(key/bind :replay ["l"] replay/cursor-right)
(key/bind :replay ["h"] replay/cursor-left)
(key/bind :replay ["j"] replay/cursor-down)
(key/bind :replay ["k"] replay/cursor-up)
(key/bind :replay ["v"] replay/select)
(key/bind :replay ["y"] replay/copy)
(key/bind :replay ["n"] replay/search-again)
(key/bind :replay ["N"] replay/search-reverse)
(key/bind :replay [" "] replay/time-play)
(key/bind :replay ["1"] (fn [&] (replay/time-playback-rate 1)))
(key/bind :replay ["2"] (fn [&] (replay/time-playback-rate 2)))
(key/bind :replay ["3"] (fn [&] (replay/time-playback-rate 5)))
(key/bind :replay ["!"] (fn [&] (replay/time-playback-rate -1)))
(key/bind :replay ["@"] (fn [&] (replay/time-playback-rate -2)))
(key/bind :replay ["#"] (fn [&] (replay/time-playback-rate -5)))
