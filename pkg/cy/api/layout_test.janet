(test "layout/new"
      (assert (deep= (layout/new (pane)) (layout/pane)))
      (assert (deep= (layout/new (attach)) (layout/pane :attached true)))
      (assert (deep=
                (layout/new (split (pane) (pane)))
                (layout/split (layout/pane) (layout/pane)))))

(test ":pane"
      (layout/set (layout/pane :id 2 :attached true))
      (layout/set (layout/pane :attached true)))

(test "layout/get"
      (def layout (layout/pane
                    :id 2
                    :remove-on-exit false
                    :attached true))
      (layout/set layout)
      (assert (deep= (layout/get) layout)))

(test "borders"
      (do
        (def layout (layout/new (split (attach) (pane)
                                       :percent 26
                                       :border :normal)))
        (layout/set layout)
        (assert (deep= (layout/get) layout)))

      (do
        (def layout (layout/new (split (attach) (pane)
                                       :percent 26
                                       :border :none)))
        (layout/set layout)
        (assert (deep= (layout/get) layout)))

      (expect-error (layout/set
                      (layout/new (split (attach) (pane)
                                         :percent 26
                                         :border :asd
                                         :vertical false)))))

(test "tabs"
      (def layout
        (layout/new
          (tabs
            @[(active-tab "pane" (attach))]
            :bottom false)))

      (layout/set layout)
      (assert (deep= (layout/get) layout)))

(test "tab actions"
      (action/new-tab)
      (action/new-tab)
      (action/new-tab)

      (action/next-tab)
      (do
        (def {:tabs tabs} (layout/get))
        (def {:active active} (tabs 0))
        (assert active))

      (action/next-tab)
      (do
        (def {:tabs tabs} (layout/get))
        (def {:active active} (tabs 1))
        (assert active))

      (action/prev-tab)
      (do
        (def {:tabs tabs} (layout/get))
        (def {:active active} (tabs 0))
        (assert active))

      (action/prev-tab)
      (do
        (def {:tabs tabs} (layout/get))
        (def {:active active} (tabs 3))
        (assert active)))

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
         :border-fg "7"
         :border-bg "7"
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
         :node {:type :pane :attached true}})

      # omit a bunch of fields
      (layout/set
        {:type :margins
         :node {:type :pane :attached true}}))

(test ":border"
      (layout/set
        {:type :borders
         :title "test"
         :border :double
         :node {:type :pane :attached true}})

      (layout/set
        {:type :borders
         :node {:type :pane :attached true}}))

(test "bad input"
      # bad node
      (expect-error (layout/set
                      {:type :margins
                       :cols 20
                       :rows 0
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
                  (layout/new
                    (tabs
                      @[(active-tab "tab" (attach))])))
                @[:tabs 0 :node]))

      (assert (deep=
                (layout/attach-path
                  {:type :margins
                   :cols 20
                   :rows 0
                   :node {:type :pane :attached true}})
                @[:node]))

      (assert (deep=
                (layout/attach-path
                  {:type :split
                   :percent 50
                   :a {:type :pane}
                   :b {:type :pane :attached true}})
                @[:b]))

      (assert (deep=
                (layout/attach-path
                  {:type :margins
                   :cols 20
                   :rows 0
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
                   :node {:type :pane :attached true}}
                  @[:node])
                {:type :pane :attached true}))

      (assert (deep=
                (layout/path
                  {:type :margins
                   :cols 20
                   :rows 0
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
                   :node {:type :pane :attached true}}
                  @[:a])
                nil)))

(test "layout/find"
      (assert (deep=
                (layout/find
                  {:type :margins
                   :cols 20
                   :rows 0
                   :node {:type :pane :attached true}}
                  |($ :attached))
                @[:node]))

      (assert (deep=
                (layout/find
                  {:type :margins
                   :cols 20
                   :rows 0
                   :node {:type :pane :attached true}}
                  |(layout/type? :margins $))
                @[])))

(test "layout/assoc"
      (assert (deep=
                (layout/assoc
                  {:type :margins
                   :cols 20
                   :rows 0
                   :node {:type :pane :attached true}}
                  @[:node]
                  {:type :pane})
                {:type :margins
                 :cols 20
                 :rows 0
                 :node {:type :pane}}))

      # Does nothing, bad path
      (assert (deep=
                (layout/assoc
                  {:type :margins
                   :cols 20
                   :rows 0
                   :node {:type :pane :attached true}}
                  @[:a]
                  {:type :pane})
                {:type :margins
                 :cols 20
                 :rows 0
                 :node {:type :pane :attached true}})))

(test "layout/detach"
      (assert (deep=
                (layout/detach (layout/new (margins (attach)
                                                    :cols 20
                                                    :rows 0)))
                (layout/new (margins (pane)
                                     :cols 20
                                     :rows 0)))))

(test "layout/split-*"
      (assert (deep=
                (layout/split-right
                  (layout/new (margins (attach) :cols 20))
                  (layout/pane :id 2 :attached true))

                (layout/new
                  (margins
                    (split (pane) (attach :id 2))
                    :cols 20))))

      (assert (deep=
                (layout/split-down (layout/split-right
                                     (layout/pane :attached true)
                                     (layout/pane :attached true))
                                   (layout/pane :id 2 :attached true))

                (layout/new (split (pane)
                                   (vsplit (pane) (attach :id 2)))))))

(test "layout/find-last"
      (def layout
        {:type :margins
         :cols 20
         :node {:type :margins
                :cols 20
                :node {:type :pane :attached true}}})

      (def path (layout/attach-path layout))

      (assert (deep=
                (layout/find-last layout path |(layout/type? :margins $))
                @[:node])))

(test "layout/move-up"
      (assert (deep=
                (layout/move-up
                  {:type :split
                   :vertical true
                   :a (layout/pane)
                   :b {:type :margins :node {:type :pane :attached true}}})

                {:type :split
                 :vertical true
                 :a {:type :pane :attached true}
                 :b {:type :margins :node (layout/pane)}}))

      (assert (deep=
                (layout/move-up
                  {:type :split
                   :vertical true
                   :a {:type :split
                       :vertical true
                       :a (layout/pane)
                       :b (layout/pane)}
                   :b {:type :split
                       :vertical true
                       :a {:type :pane :attached true}
                       :b (layout/pane)}})

                {:type :split
                 :vertical true
                 :a {:type :split
                     :vertical true
                     :a (layout/pane)
                     :b {:type :pane :attached true}}
                 :b {:type :split
                     :vertical true
                     :a (layout/pane)
                     :b (layout/pane)}})))


(test "layout/move-down"
      (assert (deep=
                (layout/move-down
                  {:type :split
                   :vertical true
                   :a {:type :pane :attached true}
                   :b {:type :margins :node (layout/pane)}})

                {:type :split
                 :vertical true
                 :a (layout/pane)
                 :b {:type :margins :node {:type :pane :attached true}}})))

(test "layout/move-left"
      (assert (deep=
                (layout/move-left
                  {:type :split
                   :a (layout/pane)
                   :b {:type :borders
                       :node {:type :pane
                              :attached true}}})

                {:type :split
                 :a {:type :pane :attached true}
                 :b {:type :borders
                     :node (layout/pane)}})))

(test "layout/map"
      (assert (deep=
                (layout/map
                  |(if (layout/pane? $) {:type :pane :id 2} $)
                  {:type :split
                   :vertical true
                   :a {:type :pane :attached true}
                   :b {:type :margins :node {:type :pane}}})

                {:type :split
                 :vertical true
                 :a {:type :pane :id 2}
                 :b {:type :margins :node {:type :pane :id 2}}})))

(test "action/set-layout-borders"
      (layout/set
        {:type :borders
         :title "test"
         :border :double
         :node {:type :pane :attached true}})

      (action/set-layout-borders))
