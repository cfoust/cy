(test "(key/remap)"
      (key/remap :root ["ctrl+a"] ["`"])
      (def before (key/get :root))
      (key/remap :root ["ctrl+a"] ["`"])
      (def after (key/get :root))
      (assert (deep= (length before) (length after))))

(test "(key/current)"
      # Should not error
      (key/current))
