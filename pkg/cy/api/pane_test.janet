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
      (assert (= (pane/current) cmd2)))


# TODO(cfoust): 07/11/24 screen test is more complicated

