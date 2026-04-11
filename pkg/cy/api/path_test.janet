(def home (os/getenv "HOME"))

(test "(path/expand) leaves plain paths alone"
  (assert (= "/tmp/foo" (path/expand "/tmp/foo")))
  (assert (= "relative/path" (path/expand "relative/path"))))

(test "(path/expand) expands bare ~"
  (assert (= home (path/expand "~"))))

(test "(path/expand) expands ~/ prefix"
  (assert (= (string home "/foo") (path/expand "~/foo")))
  (assert (= (string home "/foo/bar") (path/expand "~/foo/bar"))))

(test "(path/expand) does not expand ~user"
  (assert (= "~user/foo" (path/expand "~user/foo"))))
