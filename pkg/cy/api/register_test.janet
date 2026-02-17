(test "registers"
  (register/set "a" "a")
  (register/set "b" "b")
  (register/set "+" "test")
  (assert (= "a" (register/get "a")))
  (assert (= "b" (register/get "b")))
  (assert (= "test" (register/get "+"))))
