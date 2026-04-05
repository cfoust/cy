(test "(pane/attach)"
  (pane/attach (cmd/new :root)))

(test-no-context "(pane/attach) no client"
  (expect-error (pane/attach (cmd/new :root))))

(test "(pane/current)"
  (def cmd (cmd/new :root))
  (pane/attach cmd)
  (assert (= (pane/current) cmd)))

(test "history"
  (def cmd1 (cmd/new :root))
  (def cmd2 (cmd/new :root))
  (def cmd3 (cmd/new :root))

  (pane/attach cmd1)
  (pane/attach cmd2)
  (pane/attach cmd3)

  (pane/history-backward)
  (assert (= (pane/current) cmd2))
  (pane/history-backward)
  (assert (= (pane/current) cmd1))

  (pane/history-forward)
  (assert (= (pane/current) cmd2))

  (pane/history-forward)
  (assert (= (pane/current) cmd3))

  (tree/rm cmd2)
  (pane/history-backward)
  (assert (= (pane/current) cmd1))

  (pane/history-forward)
  (assert (= (pane/current) cmd3)))

# TODO(cfoust): 07/11/24 screen test is more complicated

(test "(action/next-pane) and (action/prev-pane)"
  (def group (group/mkdir :root "/group"))
  (def cmd1 (cmd/new group))
  (def cmd2 (cmd/new group))
  (def cmd3 (cmd/new group))
  (pane/attach cmd2)

  (action/next-pane)
  (assert (= (pane/current) cmd3))

  (action/next-pane)
  (assert (= (pane/current) cmd1))

  (action/prev-pane)
  (assert (= (pane/current) cmd3))

  (action/prev-pane)
  (assert (= (pane/current) cmd2)))

(test "(pane/screen) returns struct"
  (def pane (cmd/new :root))
  (def result (pane/screen pane))
  (assert (dictionary? result))
  (assert (indexed? (result :lines)))
  (assert (boolean? (result :is-alt))))

(test "(pane/screen) is-alt false for normal shell"
  (def pane (cmd/new :root))
  (def result (pane/screen pane))
  (assert (not (result :is-alt))))

(test "(pane/screen) with scrollback"
  (def pane (cmd/new :root))
  (def result (pane/screen pane :scrollback true))
  (assert (dictionary? result))
  (assert (indexed? (result :lines))))

(test "(pane/send-keys)"
  (def cmd (cmd/new :root))
  (pane/send-keys cmd @["test"
                        "ctrl+a"]))
