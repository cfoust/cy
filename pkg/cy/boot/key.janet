# Key binding utilities

(defn key/re
  ```Create a regex pattern for key bindings.
  
  Usage:
    (key/bind :copy ["f" (key/re ".")] replay/jump-forward)
  ```
  [pattern]
  {:type :re :pattern pattern})

(defn key/count
  ```Create a counted pattern that matches between min and max times.
  
  Usage:
    (key/bind :copy [(key/count "a" 1 3) "g"] replay/goto-line)
    (key/bind :copy [(key/count (key/re "[0-9]") 1 3) "g"] replay/goto-line)
  ```
  [pattern min max]
  {:type :count :pattern pattern :min min :max max})
