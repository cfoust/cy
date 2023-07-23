(def prefix "ctrl+a")

(def projects (group/new (tree/root)))
(def shells (group/new (tree/root)))

(key/bind
  [prefix "j"]
  "create a new shell"
  (fn [&]
    (def path (cmd/path (pane/current)))
    (def shell (cmd/new shells path))
    (pane/attach shell)))

(key/bind
  [prefix "n"]
  "create a new project"
  (fn [&]
    (def path (cmd/path (pane/current)))
    (def project (group/new projects))
    # TODO(cfoust): 07/22/23 basename
    (tree/set-name project path)
    (def editor
      (cmd/new project
               path
               :command (os/getenv "EDITOR" "vim")))
    (def shell (cmd/new project path))
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
