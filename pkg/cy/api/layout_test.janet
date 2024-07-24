(test ":pane"
      (layout/set {:type :pane :attached true :id 2})
      (layout/set {:type :pane :id 2})
      (layout/set {:type :pane}))

(test ":splits"
      (layout/set
        {:type :splits
         # horizontal
         :percent 26
         :a {:type :pane}
         :b {:type :pane}})

      (layout/set
        {:type :splits
         :vertical true
         :percent 26
         :a {:type :pane}
         :b {:type :pane}})

      (expect-error (layout/set
                      {:type :splits
                       :vertical true
                       # don't let both of these get set
                       :cells 20
                       :percent 26
                       :a {:type :pane}
                       :b {:type :pane}})))

(test ":margins"
      (layout/set
        {:type :margins
         :cols 20
         :rows 0
         :frame "big-hex"
         :node {:type :pane}})

      # omit a bunch of fields
      (layout/set
        {:type :margins
         :node {:type :pane}}))

(test "bad input"
      (expect-error (layout/set
                      {:type :margins
                       :cols 20
                       :rows 0
                       :frame "big-hex"
                       # bad node
                       :node 1213132})))
