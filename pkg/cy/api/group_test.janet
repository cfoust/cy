(test "happy path"
      (def node (group/mkdir :root "/foo/bar/baz"))
      (assert (= (tree/path node) "/foo/bar/baz")))

(test "bad path"
      (expect-error (group/mkdir :root "foo")))

(test "existing node"
      (cmd/new :root :name "foo")
      (expect-error (group/mkdir :root "/foo")))
