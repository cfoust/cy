(test "(key/remap)"
      (key/remap :root ["ctrl+a"] ["`"])
      (def before (key/get :root))
      (key/remap :root ["ctrl+a"] ["`"])
      (def after (key/get :root))
      (assert (deep= (length before) (length after))))

(test "(key/current)"
      # Should not error
      (key/current))

(test "(key/bind)"
      (key/bind :root ["alt+ctrl+a"] (fn []))
      (expect-error (key/bind :root ["alt+"] (fn []))))

(test "(key/re)"
      # Test that key/re creates a valid regex pattern
      (def pattern (key/re "[0-9]"))
      (assert (tuple? pattern))
      (assert (= (length pattern) 2))
      (assert (= (get pattern 0) :re))
      (assert (= (get pattern 1) "[0-9]")))

(test "(key/count)"
      # Test that key/count creates a valid count pattern
      (def pattern (key/count "a" 1 3))
      (assert (tuple? pattern))
      (assert (= (length pattern) 4))
      (assert (= (get pattern 0) :count))
      (assert (= (get pattern 1) "a"))
      (assert (= (get pattern 2) 1))
      (assert (= (get pattern 3) 3)))

(test "(key/count) with regex"
      # Test that key/count works with regex patterns
      (def re-pattern (key/re "[0-9]"))
      (def count-pattern (key/count re-pattern 1 3))
      (assert (tuple? count-pattern))
      (assert (= (length count-pattern) 4))
      (assert (= (get count-pattern 0) :count))
      (assert (deep= (get count-pattern 1) re-pattern))
      (assert (= (get count-pattern 2) 1))
      (assert (= (get count-pattern 3) 3)))

(test "(key/bind) with key/re"
      # Test that key/re works in bindings
      (key/bind :root [(key/re "[0-9]") "x"] (fn [digit]
        # Should receive the matched digit as an argument
        (assert (string? digit)))))

(test "(key/bind) with key/count"
      # Test that key/count works in bindings
      (key/bind :root [(key/count "a" 1 3) "g"] (fn [matches]
        # Should receive an array of matched strings
        (assert (indexed? matches))
        (assert (>= (length matches) 1))
        (assert (<= (length matches) 3))
        (each match matches
          (assert (= match "a"))))))

(test "(key/bind) with key/count and regex"
      # Test that key/count works with regex patterns in bindings
      (key/bind :root [(key/count (key/re "[0-9]") 1 3) "g"] (fn [matches])
        # Should receive an array of matched digit strings
        (assert (indexed? matches))
        (assert (>= (length matches) 1))
        (assert (<= (length matches) 3))
        (each match matches
          (assert (string? match)))))
