(test "layout/new"
  (assert (deep= (layout/new (view)) (layout/view)))
  (assert (deep= (layout/new (attach)) (layout/view :attached true)))
  (assert (deep=
            (layout/new (split (view) (view)))
            (layout/split (layout/view) (layout/view)))))

(test ":view"
  (layout/set (layout/view :id 2 :attached true))
  (layout/set (layout/view :attached true)))

(test "layout/attach-id"
  (assert (deep=
            (layout/attach-id (layout/view :id 2 :attached true))
            2))
  (assert (deep=
            (layout/attach-id (layout/view :id nil :attached true))
            nil)))

(test "layout/get"
  (def layout (layout/view
                :id 2
                :remove-on-exit false
                :attached true))
  (layout/set layout)
  (assert (deep= (layout/get) layout)))

(test "borders"
  (do
    (def layout (layout/new (split (attach) (view)
                                   :percent 26
                                   :border :normal)))
    (layout/set layout)
    (assert (deep= (layout/get) layout)))

  (do
    (def layout (layout/new (split (attach) (view)
                                   :percent 26
                                   :border :none)))
    (layout/set layout)
    (assert (deep= (layout/get) layout)))

  (expect-error (layout/set
                  (layout/new (split (attach) (view)
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
      (layout/new (tabs @[(tab "foo" (view))
                          (tab "bar" (view))]))))

  # empty name
  (expect-error (layout/set
                  (layout/new (tabs @[(active-tab "" (attach))]))))

  # nested tabs with no active tab
  (expect-error (layout/set
                  (layout/new
                    (tabs @[(active-tab "outer"
                                        (tabs @[(tab "inner" (view))]))]))))

  # multiple tabs with one active
  (layout/set (layout/new (tabs @[(tab "foo" (view))
                                  (active-tab "bar" (attach))
                                  (tab "baz" (view))])))

  # multiple active tabs should fail (exactly one active required)
  (expect-error (layout/set (layout/new (tabs @[(active-tab "foo" (attach))
                                                (active-tab "bar" (view))]))))

  # single valid tab should pass
  (layout/set (layout/new (tabs @[(active-tab "single" (attach))]))))

(test "split validation"
  # split with both children attached
  (expect-error (layout/set
                  (layout/new (split (attach) (attach)))))

  # split with no children attached
  (expect-error (layout/set
                  (layout/new (split (view) (view)))))

  # split with one child attached
  (layout/set (layout/new (split (attach) (view))))
  (layout/set (layout/new (split (view) (attach))))

  # vertical split with one child attached
  (layout/set (layout/new (vsplit (attach) (view))))
  (layout/set (layout/new (vsplit (view) (attach)))))

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
    (layout/new (split (attach) (view) :percent 26)))

  (layout/set
    (layout/new (vsplit (attach) (view)
                        :percent 26
                        :border-fg "7"
                        :border-bg "7")))

  (expect-error (layout/set
                  {:type :split
                   :vertical true
                   # don't let both of these get set
                   :cells 20
                   :percent 26
                   :a {:type :view :attached true}
                   :b {:type :view}})))

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
                   :a {:type :view :attached true}
                   :b {:type :view :attached true}})))


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
               :node {:type :view :attached true}})
            @[:node]))

  (assert (deep=
            (layout/attach-path
              {:type :split
               :percent 50
               :a {:type :view}
               :b {:type :view :attached true}})
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
                      :a {:type :view :attached true}
                      :b {:type :view}}})
            @[:node :a])))


(test "layout/path"
  (assert (deep=
            (layout/path
              {:type :margins
               :cols 20
               :rows 0
               :node {:type :view :attached true}}
              @[:node])
            {:type :view :attached true}))

  (assert (deep=
            (layout/path
              {:type :margins
               :cols 20
               :rows 0
               :node {:type :split
                      :percent 50
                      :a {:type :view :attached true}
                      :b {:type :view}}}
              @[:node :a])
            {:type :view :attached true}))

  (assert (deep=
            (layout/path
              {:type :margins
               :cols 20
               :rows 0
               :node {:type :view :attached true}}
              @[:a])
            nil)))

(test "layout/find"
  (assert (deep=
            (layout/find
              {:type :margins
               :cols 20
               :rows 0
               :node {:type :view :attached true}}
              |($ :attached))
            @[:node]))

  (assert (deep=
            (layout/find
              {:type :margins
               :cols 20
               :rows 0
               :node {:type :view :attached true}}
              |(layout/type? :margins $))
            @[])))

(test "layout/assoc"
  (assert (deep=
            (layout/assoc
              {:type :margins
               :cols 20
               :rows 0
               :node {:type :view :attached true}}
              @[:node]
              {:type :view})
            {:type :margins
             :cols 20
             :rows 0
             :node {:type :view}}))

  # Does nothing, bad path
  (assert (deep=
            (layout/assoc
              {:type :margins
               :cols 20
               :rows 0
               :node {:type :view :attached true}}
              @[:a]
              {:type :view})
            {:type :margins
             :cols 20
             :rows 0
             :node {:type :view :attached true}})))

(test "layout/detach"
  (assert (deep=
            (layout/detach (layout/new (margins (attach)
                                                :cols 20
                                                :rows 0)))
            (layout/new (margins (view)
                                 :cols 20
                                 :rows 0)))))

(test "layout/split-*"
  (assert (deep=
            (layout/split-right
              (layout/new (margins (attach) :cols 20))
              (layout/view :id 2 :attached true))

            (layout/new
              (margins
                (split (view) (attach :id 2))
                :cols 20))))

  (assert (deep=
            (layout/split-down (layout/split-right
                                 (layout/view :attached true)
                                 (layout/view :attached true))
                               (layout/view :id 2 :attached true))

            (layout/new (split (view)
                               (vsplit (view) (attach :id 2)))))))

(test "layout/find-last"
  (def layout
    {:type :margins
     :cols 20
     :node {:type :margins
            :cols 20
            :node {:type :view :attached true}}})

  (def path (layout/attach-path layout))

  (assert (deep=
            (layout/find-last layout path |(layout/type? :margins $))
            @[:node])))

(test "layout/move-up"
  (assert (deep=
            (layout/move-up
              {:type :split
               :vertical true
               :a (layout/view)
               :b {:type :margins :node {:type :view :attached true}}})

            {:type :split
             :vertical true
             :a {:type :view :attached true}
             :b {:type :margins :node (layout/view)}}))

  (assert (deep=
            (layout/move-up
              {:type :split
               :vertical true
               :a {:type :split
                   :vertical true
                   :a (layout/view)
                   :b (layout/view)}
               :b {:type :split
                   :vertical true
                   :a {:type :view :attached true}
                   :b (layout/view)}})

            {:type :split
             :vertical true
             :a {:type :split
                 :vertical true
                 :a (layout/view)
                 :b {:type :view :attached true}}
             :b {:type :split
                 :vertical true
                 :a (layout/view)
                 :b (layout/view)}})))


(test "layout/move-down"
  (assert (deep=
            (layout/move-down
              {:type :split
               :vertical true
               :a {:type :view :attached true}
               :b {:type :margins :node (layout/view)}})

            {:type :split
             :vertical true
             :a (layout/view)
             :b {:type :margins :node {:type :view :attached true}}})))

(test "layout/move-down stack"
  # Move down from the first leaf to the second;
  # both :attached and :active should move
  (assert (deep=
            (layout/move-down
              (layout/new
                (stack
                  @[(active-leaf (attach))
                    (leaf (view))])))

            (layout/new
              (stack
                @[(leaf (view))
                  (active-leaf (view :attached true))])))))

(test "layout/move-up stack"
  # Move up from the second leaf to the first;
  # both :attached and :active should move
  (assert (deep=
            (layout/move-up
              (layout/new
                (stack
                  @[(leaf (view))
                    (active-leaf (attach))])))

            (layout/new
              (stack
                @[(active-leaf (view :attached true))
                  (leaf (view))])))))

(test "layout/move-down stack three leaves"
  # Move down from first leaf, should land on second;
  # :active moves with :attached
  (assert (deep=
            (layout/move-down
              (layout/new
                (stack
                  @[(active-leaf (attach))
                    (leaf (view :id 2))
                    (leaf (view :id 3))])))

            (layout/new
              (stack
                @[(leaf (view))
                  (active-leaf (view :id 2 :attached true))
                  (leaf (view :id 3))])))))

(test "layout/move-up stack three leaves"
  # Move up from third leaf, should land on second;
  # :active moves with :attached
  (assert (deep=
            (layout/move-up
              (layout/new
                (stack
                  @[(leaf (view :id 1))
                    (leaf (view :id 2))
                    (active-leaf (attach))])))

            (layout/new
              (stack
                @[(leaf (view :id 1))
                  (active-leaf (view :id 2 :attached true))
                  (leaf (view))])))))

(test "layout/move-up stack at top"
  # Already at the top leaf, should not change
  (def layout
    (layout/new
      (stack
        @[(active-leaf (attach))
          (leaf (view :id 2))])))

  (assert (deep= (layout/move-up layout) layout)))

(test "layout/move-down stack at bottom"
  # Already at the bottom leaf, should not change
  (def layout
    (layout/new
      (stack
        @[(leaf (view :id 1))
          (active-leaf (attach))])))

  (assert (deep= (layout/move-down layout) layout)))

(test "layout/move-right tabs"
  (def layout
    (layout/new
      (tabs
        @[(active-tab "a" (attach))
          (tab "b" (view :id 1))])))

  (assert (deep=
            (layout/move-right layout)
            {:type :tabs
             :tabs @[{:name "a"
                      :node (layout/view)
                      :active false}
                     {:name "b"
                      :node {:type :view :id 1 :attached true}
                      :active true}]})))

(test "layout/move-left tabs"
  (def layout
    {:type :tabs
     :tabs @[{:name "a"
              :node (layout/view :id 1)
              :active false}
             {:name "b"
              :node {:type :view :attached true}
              :active true}]})

  (assert (deep=
            (layout/move-left layout)
            {:type :tabs
             :tabs @[{:name "a"
                      :node {:type :view :id 1 :attached true}
                      :active true}
                     {:name "b"
                      :node (layout/view)
                      :active false}]})))

(test "layout/move-right tabs three tabs"
  (def layout
    {:type :tabs
     :tabs @[{:name "a"
              :node (layout/view :id 1)
              :active false}
             {:name "b"
              :node {:type :view :attached true}
              :active true}
             {:name "c"
              :node (layout/view :id 2)
              :active false}]})

  (assert (deep=
            (layout/move-right layout)
            {:type :tabs
             :tabs @[{:name "a"
                      :node (layout/view :id 1)
                      :active false}
                     {:name "b"
                      :node (layout/view)
                      :active false}
                     {:name "c"
                      :node {:type :view :id 2 :attached true}
                      :active true}]})))

(test "layout/move-right tabs at end"
  (def layout
    (layout/new
      (tabs
        @[(tab "a" (view :id 1))
          (active-tab "b" (attach))])))

  (assert (deep= (layout/move-right layout) layout)))

(test "layout/move-left tabs at start"
  (def layout
    (layout/new
      (tabs
        @[(active-tab "a" (attach))
          (tab "b" (view :id 1))])))

  (assert (deep= (layout/move-left layout) layout)))

(test "layout/move-left"
  (assert (deep=
            (layout/move-left
              {:type :split
               :a (layout/view)
               :b {:type :borders
                   :node {:type :view
                          :attached true}}})

            {:type :split
             :a {:type :view :attached true}
             :b {:type :borders
                 :node (layout/view)}})))

(test "layout/map"
  (assert (deep=
            (layout/map
              |(if (layout/view? $) {:type :view :id 2} $)
              {:type :split
               :vertical true
               :a {:type :view :attached true}
               :b {:type :margins :node {:type :view}}})

            {:type :split
             :vertical true
             :a {:type :view :id 2}
             :b {:type :margins :node {:type :view :id 2}}})))

(test "action/set-layout-borders"
  (layout/set
    {:type :borders
     :title "test"
     :border :double
     :node {:type :view :attached true}})

  (action/set-layout-borders))

(test "layout/remove-attached"
  (assert (deep=
            (layout/remove-attached
              (layout/new
                (split
                  (attach)
                  (view :id 2))))

            (layout/new
              (attach :id 2))))

  # We keep other tabs if they're there
  (assert (deep=
            (layout/remove-attached
              (layout/new
                (tabs
                  @[(active-tab "tab" (attach))
                    (tab "tab" (view :id 2))])))

            (layout/new
              (tabs
                @[(active-tab "tab" (attach :id 2))]))))

  # Removing the last tab activates the previous one
  (assert (deep=
            (layout/remove-attached
              (layout/new
                (tabs
                  @[(tab "a" (view :id 1))
                    (tab "b" (view :id 2))
                    (active-tab "c" (attach))])))

            (layout/new
              (tabs
                @[(tab "a" (view :id 1))
                  (active-tab "b"
                              (attach :id 2))]))))

  # Removing a middle tab activates the next one
  (assert (deep=
            (layout/remove-attached
              (layout/new
                (tabs
                  @[(tab "a" (view :id 1))
                    (active-tab "b" (attach))
                    (tab "c" (view :id 3))])))

            (layout/new
              (tabs
                @[(tab "a" (view :id 1))
                  (active-tab "c"
                              (attach :id 3))])))))

(test "margins actions"
  (layout/set
    {:type :margins
     :cols 20
     :rows 0
     :border :rounded
     :node {:type :view :attached true}})

  (action/toggle-margins)
  (action/toggle-margins)
  (def {:border borders} (layout/get))
  (assert (= borders :rounded)))

(test "layout/grid empty"
  (assert (nil? (layout/grid @[])))
  (assert (nil? (layout/grid nil))))

(test "layout/grid single node"
  (def result (layout/new (layout/grid @[(view :id 1)])))
  (assert (= (result :type) :view))
  (assert (= (result :id) 1)))

(test "layout/grid two nodes"
  (def result (layout/new (layout/grid @[(view :id 1) (view :id 2)])))
  (assert (= (result :type) :split))
  (assert (not (result :vertical))))

(test "layout/grid three nodes"
  # 2 columns × 2 rows (last row has 1 node)
  (def result (layout/new (layout/grid @[(view :id 1)
                                         (view :id 2)
                                         (view :id 3)])))
  (assert (= (result :type) :split))
  (assert (result :vertical))
  (def top-row (result :a))
  (assert (= (top-row :type) :split))
  (assert (not (top-row :vertical)))
  (def bottom-row (result :b))
  (assert (= (bottom-row :type) :view))
  (assert (= (bottom-row :id) 3)))

(test "layout/grid four nodes"
  # 2×2 grid
  (def result (layout/new (layout/grid @[(view :id 1)
                                         (view :id 2)
                                         (view :id 3)
                                         (view :id 4)])))
  (assert (= (result :type) :split))
  (assert (result :vertical))
  (def top-row (result :a))
  (assert (= (top-row :type) :split))
  (assert (not (top-row :vertical)))
  (def bottom-row (result :b))
  (assert (= (bottom-row :type) :split))
  (assert (not (bottom-row :vertical))))

(test "action/new-tab auto-numbering"
  # First call wraps into tabs with "1" and "2"
  (action/new-tab)
  (do
    (def {:tabs tabs} (layout/get))
    (assert (= (length tabs) 2))
    (assert (= ((tabs 0) :name) "1"))
    (assert (= ((tabs 1) :name) "2"))
    (assert ((tabs 1) :active)))

  # Second call adds tab "3"
  (action/new-tab)
  (do
    (def {:tabs tabs} (layout/get))
    (assert (= (length tabs) 3))
    (assert (= ((tabs 0) :name) "1"))
    (assert (= ((tabs 1) :name) "2"))
    (assert (= ((tabs 2) :name) "3"))
    (assert ((tabs 2) :active)))

  # Close the active tab ("3"), then close "2"
  (action/close-tab)
  (action/close-tab)

  # Only tab "1" left
  (do
    (def {:tabs tabs} (layout/get))
    (assert (= (length tabs) 1))
    (assert (= ((tabs 0) :name) "1")))

  # Adding a tab should reuse "2" since it's now free
  (action/new-tab)
  (do
    (def {:tabs tabs} (layout/get))
    (assert (= (length tabs) 2))
    (assert (= ((tabs 0) :name) "1"))
    (assert (= ((tabs 1) :name) "2"))))

(test "action/grow-split and action/shrink-split"
  # Percent: side A grow/shrink
  (layout/set (layout/new (split (attach) (view))))
  (action/grow-split)
  (assert (= ((layout/get) :percent) 60))

  (layout/set (layout/new (split (attach) (view))))
  (action/shrink-split)
  (assert (= ((layout/get) :percent) 40))

  # Percent: side B flips direction
  (layout/set (layout/new (split (view) (attach))))
  (action/grow-split)
  (assert (= ((layout/get) :percent) 40))

  (layout/set (layout/new (split (view) (attach))))
  (action/shrink-split)
  (assert (= ((layout/get) :percent) 60))

  # Cells mode
  (layout/set (layout/new (split (attach) (view) :cells 20)))
  (action/grow-split)
  (assert (= ((layout/get) :cells) 23))

  (layout/set (layout/new (split (attach) (view) :cells 20)))
  (action/shrink-split)
  (assert (= ((layout/get) :cells) 17))

  # Percent clamping
  (layout/set (layout/new (split (attach) (view) :percent 95)))
  (action/grow-split)
  (assert (= ((layout/get) :percent) 99))

  (layout/set (layout/new (split (attach) (view) :percent 5)))
  (action/shrink-split)
  (assert (= ((layout/get) :percent) 1))

  # Cells clamping
  (layout/set (layout/new (split (attach) (view) :cells 2)))
  (action/shrink-split)
  (assert (= ((layout/get) :cells) 1))

  # No parent split - no-op
  (layout/set (layout/new (margins (attach))))
  (def layout-before (layout/get))
  (action/grow-split)
  (assert (deep= layout-before (layout/get)))

  # Nested: affects innermost split
  (layout/set
    (layout/new
      (split
        (view)
        (vsplit (attach) (view) :percent 30))))
  (action/grow-split)
  (assert (= (((layout/get) :b) :percent) 40)))

(test "layout/get-meta"
  (def layout
    {:type :margins
     :meta "outer"
     :node {:type :borders
            :meta "inner"
            :node {:type :view :attached true}}})

  (def path (layout/attach-path layout))

  # returns [path meta] for the nearest ancestor with :meta
  (assert (deep=
            (layout/get-meta layout path (fn [&] true))
            [@[:node] "inner"]))

  # predicate can filter by type
  (assert (deep=
            (layout/get-meta layout path
                             |(layout/type? :margins $))
            [@[] "outer"]))

  # returns nil when predicate matches nothing
  (assert (nil? (layout/get-meta layout path
                                 |(layout/type? :split $))))

  # returns nil when no nodes have :meta
  (assert (nil? (layout/get-meta
                  {:type :margins
                   :node {:type :view :attached true}}
                  @[:node]
                  (fn [&] true)))))

(test "layout/set-meta"
  (def layout
    {:type :margins
     :meta "outer"
     :node {:type :borders
            :meta "inner"
            :node {:type :view :attached true}}})

  # set meta by path
  (assert (deep=
            (layout/set-meta layout @[:node] "updated")
            {:type :margins
             :meta "outer"
             :node {:type :borders
                    :meta "updated"
                    :node {:type :view :attached true}}}))

  (assert (deep=
            (layout/set-meta layout @[] "updated")
            {:type :margins
             :meta "updated"
             :node {:type :borders
                    :meta "inner"
                    :node {:type :view :attached true}}}))

  # invalid path returns original layout
  (assert (deep= (layout/set-meta layout @[:a] "x") layout))

  # round-trip with get-meta
  (def [found-path _] (layout/get-meta layout
                                       (layout/attach-path layout)
                                       (fn [&] true)))
  (assert (deep=
            (layout/set-meta layout found-path "updated")
            {:type :margins
             :meta "outer"
             :node {:type :borders
                    :meta "updated"
                    :node {:type :view :attached true}}})))
