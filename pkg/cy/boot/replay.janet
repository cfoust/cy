(defn
  replay/copy-default
  "Yank the selection into the default register."
  []
  (replay/copy ""))

(defn
  replay/copy-clipboard
  "Yank the selection into the system clipboard."
  []
  (replay/copy "+"))
