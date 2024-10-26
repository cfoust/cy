(test "clipboard"
    (clipboard/set "test")
    (assert (= "test" (clipboard/get))))
