(test "frame"
      (input/find @[["test" {:type :frame :name "big-hex"} 2]]))

(test "animation"
      (input/find @[["test" {:type :animation :name "midjo"} 2]]))

(test "layout"
      (input/find @[["test" {:type :layout
                             :layout {:type :borders
                                      :title "test"
                                      :border :double
                                      :node {:type :pane :attached true}}} 2]]))
