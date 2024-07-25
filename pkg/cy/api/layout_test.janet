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


(test "layout/attach-path"
      (assert (deep=
                (layout/attach-path
                  {:type :margins
                   :cols 20
                   :rows 0
                   :frame "big-hex"
                   :node {:type :pane :attached true}})
                @[:node]))

      (assert (deep=
                (layout/attach-path
                  {:type :margins
                   :cols 20
                   :rows 0
                   :frame "big-hex"
                   :node {:type :split
                          :vertical true
                          :cells 20
                          :percent 26
                          :a {:type :pane :attached true}
                          :b {:type :pane}}})
                @[:node :a])))


(test "layout/path"
      (assert (deep=
                (layout/path
                  {:type :margins
                   :cols 20
                   :rows 0
                   :frame "big-hex"
                   :node {:type :pane :attached true}}
                  @[:node])
                {:type :pane :attached true}))

      (assert (deep=
                (layout/path
                  {:type :margins
                   :cols 20
                   :rows 0
                   :frame "big-hex"
                   :node {:type :split
                          :percent 50
                          :a {:type :pane :attached true}
                          :b {:type :pane}}}
                  @[:node :a])
                {:type :pane :attached true}))

      (assert (deep=
                (layout/path
                  {:type :margins
                   :cols 20
                   :rows 0
                   :frame "big-hex"
                   :node {:type :pane :attached true}}
                  @[:a])
                nil)))

(test "layout/assoc"
      (assert (deep=
                (layout/assoc
                  {:type :margins
                   :cols 20
                   :rows 0
                   :frame "big-hex"
                   :node {:type :pane :attached true}}
                  @[:node]
                  {:type :pane})
                {:type :margins
                 :cols 20
                 :rows 0
                 :frame "big-hex"
                 :node {:type :pane}}))

      # Does nothing, bad path
      (assert (deep=
                (layout/assoc
                  {:type :margins
                   :cols 20
                   :rows 0
                   :frame "big-hex"
                   :node {:type :pane :attached true}}
                  @[:a]
                  {:type :pane})
                {:type :margins
                 :cols 20
                 :rows 0
                 :frame "big-hex"
                 :node {:type :pane :attached true}})))
