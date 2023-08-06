(def prefix "ctrl+a")

(def projects (group/new (tree/root) :name "projects"))
(def shells (group/new (tree/root) :name "shells"))

(key/bind
  [prefix "j"]
  "create a new shell"
  (fn [&]
    (def path (cmd/path (pane/current)))
    (def shell (cmd/new shells path :name (path/base path)))
    (pane/attach shell)))

(key/bind
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
  [prefix "l"]
  "jump to a shell"
  (fn [&]
    (-?>>
      (group/children shells)
      (map |(tuple (cmd/path $) $))
      (fzf/find)
      (pane/attach))))

(key/bind
  [prefix "g"]
  "toggle size"
  (fn [&]
    (def size (frame/size))
    (case (+ (size 0) (size 1))
      0 (frame/set-size [0 80])
      (frame/set-size [0 0]))))

(key/bind
  [prefix "1"]
  "set size to 80 columns"
  (fn [&]
    (frame/set-size [0 80])))

(key/bind
  [prefix "2"]
  "set size to 160 columns"
  (fn [&]
    (frame/set-size [0 160])))

(key/bind
  [prefix "q"]
  "kill the cy server"
  (fn [&]
    (cy/kill-server)))

(key/bind
  [prefix "d"]
  "detach from the cy server"
  (fn [&]
    (cy/detach)))

(key/bind
  [prefix "p"]
  "enter copy mode"
  (fn [&]
    (cy/copy-mode)))

(key/bind
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
