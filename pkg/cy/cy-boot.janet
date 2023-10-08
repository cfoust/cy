(def prefix "ctrl+a")

(def projects (group/new (tree/root) :name "projects"))
(def shells (group/new (tree/root) :name "shells"))

(key/bind
  :root
  [prefix "j"]
  "create a new shell"
  (fn [&]
    (def path (cmd/path (pane/current)))
    (def shell (cmd/new shells path :name (path/base path)))
    (pane/attach shell)))

(key/bind
  :root
  [prefix "n"]
  "create a new project"
  (fn [&]
    (def path (cmd/path (pane/current)))
    (def project (group/new projects :name (path/base path)))
    (def editor
      (cmd/new project
               path
               :name "editor"
               :command (os/getenv "EDITOR" "vim")))
    (def shell (cmd/new project path :name "shell"))
    (pane/attach editor)))

(key/bind
  :root
  [prefix "k"]
  "jump to a project"
  (fn [&]
    (-?>>
      (group/children projects)
      (map |(tuple (tree/name $) $))
      (fzf/find)
      (group/children)
      (0) # Gets the first index, the editor
      (pane/attach))))

(key/bind
  :root
  [prefix "x"]
  "kill the current pane"
  (fn [&]
    (tree/kill (pane/current))))

(key/bind
  :root
  [prefix "l"]
  "jump to a shell"
  (fn [&]
    (-?>>
      (group/children shells)
      (map |(tuple (cmd/path $) $))
      (fzf/find)
      (pane/attach))))

(key/bind
  :root
  ["ctrl+l"]
  "move to the next pane"
  (fn [&]
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
    (pane/attach next)))

(key/bind
  :root
  [prefix "g"]
  "toggle size"
  (fn [&]
    (def size (frame/size))
    (case (+ (size 0) (size 1))
      0 (frame/set-size [0 80])
      (frame/set-size [0 0]))))

(key/bind
  :root
  [prefix "1"]
  "set size to 80 columns"
  (fn [&]
    (frame/set-size [0 80])))

(key/bind
  :root
  [prefix "2"]
  "set size to 160 columns"
  (fn [&]
    (frame/set-size [0 160])))

(key/bind
  :root
  [prefix "+"]
  "decrease margins by 5 columns"
  (fn [&]
    (def [lines cols] (frame/size))
    (frame/set-size [lines (+ cols 10)])))

(key/bind
  :root
  [prefix "-"]
  "increase margins by 5 columns"
  (fn [&]
    (def [lines cols] (frame/size))
    (frame/set-size [lines (- cols 10)])))

(key/bind :root [prefix "q"] "kill the cy server" (fn [&] (cy/kill-server)))
(key/bind :root [prefix "d"] "detach from the cy server" (fn [&] (cy/detach)))
(key/bind :root [prefix "p"] "enter replay mode" (fn [&] (cy/replay)))

# should actions just be functions with docstrings?
#(key/action increase-margins "increase margins by 5 columns"
            #(def [lines cols] (frame/size))
            #(frame/set-size [lines (- cols 10)]))
#(key/bind [prefix "-"] increase-margins)

#(key/bind :replay ["q"] (replay/quit))
#(key/bind :replay ["ctrl+c"] (replay/quit))
#(key/bind :replay ["esc"] (replay/quit))
#(key/bind :replay ["right"] (replay/time-step-forward))
#(key/bind :replay ["left"] (replay/time-step-backward))
#(key/bind :replay ["up"] (replay/scroll-up))
#(key/bind :replay ["down"] (replay/scroll-down))
#(key/bind :replay ["ctrl+u"] (replay/half-page-up))
#(key/bind :replay ["ctrl+d"] (replay/half-page-down))
#(key/bind :replay ["s"] (replay/start-search))
#(key/bind :replay ["g" "g"] (replay/time-beginning))
#(key/bind :replay ["G"] (replay/time-end))
