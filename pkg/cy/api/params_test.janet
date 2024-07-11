(test "no keyword"
      (expect-error (param/set :root 2 2)))

(test "invalid parameter"
      (expect-error (param/set :root :data-directory 2)))
