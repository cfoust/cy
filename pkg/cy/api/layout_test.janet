(test ":pane"
      (layout/set {:type :pane :attached true :id 2})
      (layout/set {:type :pane :id 2 :attached true})
      (layout/set {:type :pane :attached true}))

(test ":pane 2"
      (layout/set {:type :pane :attached true}))

(test ":split"
      (layout/set
        {:type :split
         # horizontal
         :percent 26
         :a {:type :pane :attached true}
         :b {:type :pane}})

      (layout/set
        {:type :split
         :vertical true
         :percent 26
         :a {:type :pane :attached true}
         :b {:type :pane}})

      (expect-error (layout/set
                      {:type :split
                       :vertical true
                       # don't let both of these get set
                       :cells 20
                       :percent 26
                       :a {:type :pane :attached true}
                       :b {:type :pane}})))

(test ":margins"
      (layout/set
        {:type :margins
         :cols 20
         :rows 0
         :frame "big-hex"
         :node {:type :pane :attached true}})

      # omit a bunch of fields
      (layout/set
        {:type :margins
         :node {:type :pane :attached true}}))

(test "bad input"
      # bad node
      (expect-error (layout/set
                      {:type :margins
                       :cols 20
                       :rows 0
                       :frame "big-hex"
                       :node 1213132}))

      (expect-error (layout/set
                      {:type :split
                       :vertical true
                       :cells 20
                       :percent 26
                       :a {:type :pane :attached true}
                       :b {:type :pane :attached true}})))
