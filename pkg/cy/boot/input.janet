(defn input/ok?
  ```Prompt the user to accept an operation with y/n.
  Returns true if the user presses 'y' or 'Y', false if they press 'n' or 'N', 
  and nil if they cancel or press any other key.```
  [prompt]
  (let [full-prompt (string prompt " (y/n)")]
    (case (input/text full-prompt :single true)
      "y" true
      "Y" true
      "n" false
      "N" false
      nil)))
