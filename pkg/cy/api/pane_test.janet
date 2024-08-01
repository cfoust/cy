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

      (tree/kill cmd2)
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
