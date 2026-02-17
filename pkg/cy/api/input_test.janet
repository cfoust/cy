(test "tuple"
  (assert (= (input/find @[["test" 2] ["test" 3]]) 2))
  (assert (= (input/find @[["test" "2"] ["test" "3"]]) "2")))

(test "frame"
  (input/find @[["test" {:type :frame :name "big-hex"} 2]]))

(test "animation"
  (input/find @[["test" {:type :animation :name "maelstrom"} 2]]))

(test "layout"
  (input/find @[["test" {:type :layout
                         :layout {:type :borders
                                  :title "test"
                                  :border :double
                                  :node {:type :pane :attached true}}} 2]]))

(test "text"
  # In test mode, this should return "test" due to SkipInput() behavior
  (assert (= "test" (input/text "Enter text:"))))

(test "single char"
  (assert (= "test" (input/text "Press any key:" :single true))))
