(test "frame"
      (input/find @[["test" {:type :frame :name "big-hex"} 2]]))

(test "animation"
      (input/find @[["test" {:type :animation :name "midjo"} 2]]))
