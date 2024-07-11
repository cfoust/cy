(test "(tree/group?) and (tree/pane?)"
      (def group (group/new :root))
      (assert (tree/group? group))
      (assert (not (tree/pane? group)))
      (def cmd (cmd/new :root))
      (assert (not (tree/group? cmd)))
      (assert (tree/pane? cmd)))

(test "(tree/set-name) and (tree/name)"
      (def group (group/new :root))
      (tree/set-name group "test")
      (assert (= "test" (tree/name group))))

(test "(tree/path)"
      (def group (group/new :root))
      (tree/set-name group "foo")
      (assert (= "/foo" (tree/path group))))

(test "(tree/parent) and (tree/root)"
      (def group (group/new :root))
      (assert (= (tree/root) (tree/parent group))))
