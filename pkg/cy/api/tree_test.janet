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

(test "(tree/id) group"
  (def node (group/mkdir :root "/foo/bar/baz"))
  (assert (= node (tree/id :root "/foo/bar/baz"))))

(test "(tree/id) pane"
  (group/mkdir :root "/foo")
  (def pane (cmd/new (tree/id :root "/foo") :name "bar"))
  (assert (= pane (tree/id :root "/foo/bar"))))

(test "(tree/id) missing"
  (expect-error (tree/id :root "/nonexistent")))

(test "(tree/id) bad path"
  (expect-error (tree/id :root "foo")))

(test "(tree/exists?) true for existing path"
  (group/mkdir :root "/foo/bar")
  (assert (tree/exists? :root "/foo/bar")))

(test "(tree/exists?) true for pane"
  (group/mkdir :root "/test")
  (cmd/new (tree/id :root "/test") :name "mypane")
  (assert (tree/exists? :root "/test/mypane")))

(test "(tree/exists?) false for missing path"
  (assert (not (tree/exists? :root "/nonexistent/path"))))

(test "(tree/exists?) false for bad path"
  (assert (not (tree/exists? :root "badpath"))))
