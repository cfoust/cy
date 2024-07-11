(test "(pane/attach)"
      (pane/attach (cmd/new :root)))

(test-no-context "(pane/attach) no client"
                 (expect-error (pane/attach (cmd/new :root))))

(test "(pane/current)"
      (def cmd (cmd/new :root))
      (pane/attach cmd)
      (assert (= (pane/current) cmd)))

# TODO(cfoust): 07/11/24 screen test is more complicated
