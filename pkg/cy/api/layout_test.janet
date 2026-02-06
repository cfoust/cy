(test "layout/new"
      (assert (deep= (layout/new (pane)) (layout/pane)))
      (assert (deep= (layout/new (attach)) (layout/pane :attached true)))
      (assert (deep=
                (layout/new (split (pane) (pane)))
                (layout/split (layout/pane) (layout/pane)))))

(test ":pane"
      (layout/set (layout/pane :id 2 :attached true))
      (layout/set (layout/pane :attached true)))

(test "layout/attach-id"
      (assert (deep=
                (layout/attach-id (layout/pane :id 2 :attached true))
                2))
      (assert (deep=
                (layout/attach-id (layout/pane :id nil :attached true))
                nil)))

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
      (assert (deep= (layout/get) layout))

      # empty tabs
      (expect-error (layout/set (layout/new (tabs @[]))))

      # no active tab
      (expect-error
        (layout/set
          (layout/new (tabs @[(tab "foo" (pane))
                              (tab "bar" (pane))]))))

      # empty name
      (expect-error (layout/set
                      (layout/new (tabs @[(active-tab "" (attach))]))))

      # nested tabs with no active tab
      (expect-error (layout/set
                      (layout/new
                        (tabs @[(active-tab "outer"
                                            (tabs @[(tab "inner" (pane))]))]))))

      # multiple tabs with one active
      (layout/set (layout/new (tabs @[(tab "foo" (pane))
                                      (active-tab "bar" (attach))
                                      (tab "baz" (pane))])))

      # multiple active tabs should fail (exactly one active required)
      (expect-error (layout/set (layout/new (tabs @[(active-tab "foo" (attach))
                                                    (active-tab "bar" (pane))]))))

      # single valid tab should pass
      (layout/set (layout/new (tabs @[(active-tab "single" (attach))]))))

(test "split validation"
      # split with both children attached
      (expect-error (layout/set
                      (layout/new (split (attach) (attach)))))

      # split with no children attached
      (expect-error (layout/set
                      (layout/new (split (pane) (pane)))))

      # split with one child attached
      (layout/set (layout/new (split (attach) (pane))))
      (layout/set (layout/new (split (pane) (attach))))

      # vertical split with one child attached
      (layout/set (layout/new (vsplit (attach) (pane))))
      (layout/set (layout/new (vsplit (pane) (attach)))))

(test "bar"
      (def layout
        (layout/new
          (bar
            |(string "asd")
            (attach)
            :bottom true)))

      (layout/set layout)
      (assert (deep= (layout/get) layout)))

(test "invalid bar"
      (def layout
        (layout/new
          {:type :bar
           :node (attach)}))

      (expect-error (layout/set layout)))

(test "color-map"
      (def layout
        (layout/new
          (color-map
            @{}
            (attach))))

      (layout/set layout)
      (assert (deep= (layout/get) layout)))

(test "invalid color-map"
      (def layout
        (layout/new
          (color-map
            ""
            (attach))))

      (expect-error (layout/set layout)))

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
        (assert active))

      (action/close-tab)
      (do
        (def {:tabs tabs} (layout/get))
        (def {:active active} (tabs 2))
        (assert active))
      (action/close-tab)
      (action/close-tab)

      # One tab left
      (do
        (def {:tabs tabs} (layout/get))
        (def {:active active} (tabs 0))
        (assert active))

      (action/close-tab)
      # Zero, should just be node
      (def {:type node-type} (layout/get))
      (assert (= node-type :margins)))

(test ":split"
      (layout/set
        (layout/new (split (attach) (pane) :percent 26)))

      (layout/set
        (layout/new (vsplit (attach) (pane)
                            :percent 26
                            :border-fg "7"
                            :border-bg "7")))

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
        (layout/new (margins (attach) :cols 20 :rows 0)))

      # omit a bunch of fields
      (layout/set
        (layout/new (margins (attach)))))

(test ":border"
      (layout/set
        (layout/new (borders (attach) :title "test" :border :double)))

      (layout/set
        (layout/new (borders (attach)))))

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

(test "layout/move-down stack"
      # Move down from the first leaf to the second;
      # both :attached and :active should move
      (assert (deep=
                (layout/move-down
                  (layout/new
                    (stack
                      @[(active-leaf (attach))
                        (leaf (pane))])))

                (layout/new
                  (stack
                    @[(leaf (pane))
                      (active-leaf (pane :attached true))])))))

(test "layout/move-up stack"
      # Move up from the second leaf to the first;
      # both :attached and :active should move
      (assert (deep=
                (layout/move-up
                  (layout/new
                    (stack
                      @[(leaf (pane))
                        (active-leaf (attach))])))

                (layout/new
                  (stack
                    @[(active-leaf (pane :attached true))
                      (leaf (pane))])))))

(test "layout/move-down stack three leaves"
      # Move down from first leaf, should land on second;
      # :active moves with :attached
      (assert (deep=
                (layout/move-down
                  (layout/new
                    (stack
                      @[(active-leaf (attach))
                        (leaf (pane :id 2))
                        (leaf (pane :id 3))])))

                (layout/new
                  (stack
                    @[(leaf (pane))
                      (active-leaf (pane :id 2 :attached true))
                      (leaf (pane :id 3))])))))

(test "layout/move-up stack three leaves"
      # Move up from third leaf, should land on second;
      # :active moves with :attached
      (assert (deep=
                (layout/move-up
                  (layout/new
                    (stack
                      @[(leaf (pane :id 1))
                        (leaf (pane :id 2))
                        (active-leaf (attach))])))

                (layout/new
                  (stack
                    @[(leaf (pane :id 1))
                      (active-leaf (pane :id 2 :attached true))
                      (leaf (pane))])))))

(test "layout/move-up stack at top"
      # Already at the top leaf, should not change
      (def layout
        (layout/new
          (stack
            @[(active-leaf (attach))
              (leaf (pane :id 2))])))

      (assert (deep= (layout/move-up layout) layout)))

(test "layout/move-down stack at bottom"
      # Already at the bottom leaf, should not change
      (def layout
        (layout/new
          (stack
            @[(leaf (pane :id 1))
              (active-leaf (attach))])))

      (assert (deep= (layout/move-down layout) layout)))

(test "layout/move-right tabs"
      (def layout
        (layout/new
          (tabs
            @[(active-tab "a" (attach))
              (tab "b" (pane :id 1))])))

      (assert (deep=
                (layout/move-right layout)
                {:type :tabs
                 :tabs @[{:name "a"
                          :node (layout/pane)
                          :active false}
                         {:name "b"
                          :node {:type :pane :id 1 :attached true}
                          :active true}]})))

(test "layout/move-left tabs"
      (def layout
        {:type :tabs
         :tabs @[{:name "a"
                  :node (layout/pane :id 1)
                  :active false}
                 {:name "b"
                  :node {:type :pane :attached true}
                  :active true}]})

      (assert (deep=
                (layout/move-left layout)
                {:type :tabs
                 :tabs @[{:name "a"
                          :node {:type :pane :id 1 :attached true}
                          :active true}
                         {:name "b"
                          :node (layout/pane)
                          :active false}]})))

(test "layout/move-right tabs three tabs"
      (def layout
        {:type :tabs
         :tabs @[{:name "a"
                  :node (layout/pane :id 1)
                  :active false}
                 {:name "b"
                  :node {:type :pane :attached true}
                  :active true}
                 {:name "c"
                  :node (layout/pane :id 2)
                  :active false}]})

      (assert (deep=
                (layout/move-right layout)
                {:type :tabs
                 :tabs @[{:name "a"
                          :node (layout/pane :id 1)
                          :active false}
                         {:name "b"
                          :node (layout/pane)
                          :active false}
                         {:name "c"
                          :node {:type :pane :id 2 :attached true}
                          :active true}]})))

(test "layout/move-right tabs at end"
      (def layout
        (layout/new
          (tabs
            @[(tab "a" (pane :id 1))
              (active-tab "b" (attach))])))

      (assert (deep= (layout/move-right layout) layout)))

(test "layout/move-left tabs at start"
      (def layout
        (layout/new
          (tabs
            @[(active-tab "a" (attach))
              (tab "b" (pane :id 1))])))

      (assert (deep= (layout/move-left layout) layout)))

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

(test "layout/remove-attached"
      (assert (deep=
                (layout/remove-attached
                  (layout/new
                    (split
                      (attach)
                      (pane :id 2))))

                (layout/new
                  (attach :id 2))))

      # We keep other tabs if they're there
      (assert (deep=
                (layout/remove-attached
                  (layout/new
                    (tabs
                      @[(active-tab "tab" (attach))
                        (tab "tab" (pane :id 2))])))

                (layout/new
                  (tabs
                    @[(active-tab "tab" (attach :id 2))])))))

(test "margins actions"
      (layout/set
        {:type :margins
         :cols 20
         :rows 0
         :border :rounded
         :node {:type :pane :attached true}})

      (action/toggle-margins)
      (action/toggle-margins)
      (def {:border borders} (layout/get))
      (assert (= borders :rounded)))

(test "layout/grid empty"
      (assert (nil? (layout/grid @[])))
      (assert (nil? (layout/grid nil))))

(test "layout/grid single node"
      (def result (layout/new (layout/grid @[(pane :id 1)])))
      (assert (= (result :type) :pane))
      (assert (= (result :id) 1)))

(test "layout/grid two nodes"
      (def result (layout/new (layout/grid @[(pane :id 1) (pane :id 2)])))
      (assert (= (result :type) :split))
      (assert (not (result :vertical))))

(test "layout/grid three nodes"
      # 2 columns × 2 rows (last row has 1 node)
      (def result (layout/new (layout/grid @[(pane :id 1)
                                             (pane :id 2)
                                             (pane :id 3)])))
      (assert (= (result :type) :split))
      (assert (result :vertical))
      (def top-row (result :a))
      (assert (= (top-row :type) :split))
      (assert (not (top-row :vertical)))
      (def bottom-row (result :b))
      (assert (= (bottom-row :type) :pane))
      (assert (= (bottom-row :id) 3)))

(test "layout/grid four nodes"
      # 2×2 grid
      (def result (layout/new (layout/grid @[(pane :id 1)
                                             (pane :id 2)
                                             (pane :id 3)
                                             (pane :id 4)])))
      (assert (= (result :type) :split))
      (assert (result :vertical))
      (def top-row (result :a))
      (assert (= (top-row :type) :split))
      (assert (not (top-row :vertical)))
      (def bottom-row (result :b))
      (assert (= (bottom-row :type) :split))
      (assert (not (bottom-row :vertical))))
