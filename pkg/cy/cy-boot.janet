(def prefix "ctrl+a")

(def shells (group/new (group/root)))

(key/bind
  [prefix "j"]
  "create a new shell"
  (fn [&]
    (def path (pane/path (pane/current)))
    (def shell (pane/new shells path))
    (pane/attach shell)))
