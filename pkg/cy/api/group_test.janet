(test "(group/mkdir) happy path"
      (def node (group/mkdir :root "/foo/bar/baz"))
      (assert (= (tree/path node) "/foo/bar/baz")))

(test "(group/mkdir) bad path"
      (expect-error (group/mkdir :root "foo")))

(test "(group/mkdir) existing node"
      (cmd/new :root :name "foo")
      (expect-error (group/mkdir :root "/foo")))

(test "(group/new)"
      (group/new :root)
      (def named (group/new :root :name "bar"))
      (assert (= (tree/path named) "/bar")))

(test "(group/children)"
      (def group (group/new :root))
      (group/new group)
      (group/new group)
      (group/new group)
      (assert (= (length (group/children group)) 3)))

(test "(action/new-project) replaces existing"
      (def initial (cmd/new :root :path (os/cwd)))
      (pane/attach initial)
      (action/new-project)
      (def projects (group/mkdir :root "/projects"))
      (assert (= (length (group/children projects)) 1))
      (action/new-project)
      (assert (= (length (group/children projects)) 1)))

(test "(group/leaves)"
      (def group (group/new :root))
      (def subgroup (group/new group))
      (cmd/new group :name "foo")
      (cmd/new subgroup :name "foo")
      (assert (= (length (group/leaves group)) 2)))
