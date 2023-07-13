(def prefix "ctrl+a")

(def shells (group/new (tree/root)))

(key/bind
  [prefix "j"]
  "create a new shell"
  (fn [&]
    (def path (cmd/path (pane/current)))
    (def shell (cmd/new shells path))
    (pane/attach shell)))

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
