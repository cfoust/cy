(def prefix "ctrl+a")

(def projects (group/new (tree/root) :name "projects"))
(def shells (group/new (tree/root) :name "shells"))

(def- actions @[])

(defmacro key/def
  "register an action"
  [name docstring & body]
  ~(upscope
     (defn ,name ,docstring [] ,;body)
     (,array/push actions [,docstring ,name])))

(key/def
  action/command-palette
  "open command palette"
  (as?-> actions _
         (input/find _ :prompt "search: actions")
         (apply _)))

(key/def
  action/new-shell
  "create a new shell"
  (def path (cmd/path (pane/current)))
  (def shell (cmd/new shells path :name (path/base path)))
  (pane/attach shell))

(key/def
  action/new-project
  "create a new project"
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
  action/jump-project
  "jump to a project"
  (as?-> projects _
         (group/children _)
         (map |(tuple (tree/name $) [:node [((group/children $) 0)]] $) _)
         (input/find _ :prompt "search: project")
         (group/children _)
         (_ 0) # Gets the first index, the editor
         (pane/attach _)))

(key/def
  action/jump-shell
  "jump to a shell"
  (as?-> (group/children shells) _
         (map |(tuple (cmd/path $) [:node [$]] $) _)
         (input/find _ :prompt "search: shell")
         (pane/attach _)))

(key/def
  action/next-pane
  "move to the next pane"
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
  action/jump-pane
  "jump to a pane"
  (as?-> (group/leaves (tree/root)) _
         (map |(tuple (tree/path $) [:node [$]] $) _)
         (input/find _ :prompt "search: pane")
         (pane/attach _)))

(key/def
  action/kill-current-pane
  "kill the current pane"
  (tree/kill (pane/current)))

(key/def
  action/toggle-margins
  "toggle margins"
  (def size (viewport/size))
  (case (+ (size 0) (size 1))
    0 (viewport/set-size [0 80])
    (viewport/set-size [0 0])))

(key/def
  action/margins-80
  "set size to 80 columns"
  (viewport/set-size [0 80]))

(key/def
  action/margins-160
  "set size to 160 columns"
  (viewport/set-size [0 160]))

(key/def
  action/choose-frame
  "choose a frame"
  (as?-> (viewport/get-frames) _
         (input/find _ :prompt "search: frame")
         (viewport/set-frame _)))

(key/def
  action/random-frame
  "switch to a random frame"
  (def frames (viewport/get-frames))
  (def rng (math/rng))
  (viewport/set-frame (get frames (math/rng-int rng (length frames)))))

(key/def
  action/margins-smaller
  "decrease margins by 5 columns"
  (def [lines cols] (viewport/size))
  (viewport/set-size [lines (+ cols 10)]))

(key/def
  action/margins-bigger
  "increase margins by 5 columns"
  (def [lines cols] (viewport/size))
  (viewport/set-size [lines (- cols 10)]))

(key/def
  action/open-log
  "open an existing log file"
  (as?-> (path/glob (path/join [(cy/get :data-dir) "*.borg"])) _
         (map |(tuple $ [:replay [$]] $) _)
         (input/find _ :prompt "search: log file")
         (replay/open (tree/root) _)
         (pane/attach _)))

(key/bind :root [prefix "j"] action/new-shell)
(key/bind :root [prefix "n"] action/new-project)
(key/bind :root [prefix "k"] action/jump-project)
(key/bind :root [prefix "l"] action/jump-shell)
(key/bind :root ["ctrl+l"] action/next-pane)

(key/bind :root [prefix ";"] action/jump-pane)
(key/bind :root [prefix "ctrl+p"] action/command-palette)
(key/bind :root [prefix "x"] action/kill-current-pane)
(key/bind :root [prefix "g"] action/toggle-margins)
(key/bind :root [prefix "1"] action/margins-80)
(key/bind :root [prefix "2"] action/margins-160)
(key/bind :root [prefix "+"] action/margins-smaller)
(key/bind :root [prefix "-"] action/margins-bigger)
(key/bind :root [prefix "r" "r"] action/random-frame)
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
(key/bind :replay [";"] replay/jump-again)
(key/bind :replay [","] replay/jump-reverse)
(key/bind :replay ["f" [:re "."]] replay/jump-forward)
(key/bind :replay ["F" [:re "."]] replay/jump-backward)
(key/bind :replay ["t" [:re "."]] replay/jump-to-forward)
(key/bind :replay ["T" [:re "."]] replay/jump-to-backward)
