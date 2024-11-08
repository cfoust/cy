(defn
  clipboard/get
  "Get the contents of the system clipboard."
  []
  (register/get "+"))

(defn
  clipboard/set
  "Set the contents of the system clipboard."
  [text]
  (register/set "+" text))

(defn
  register/insert
  "Insert the contents of the given register in the current pane."
  [register]
  (pane/send-keys
    (pane/current)
    @[(register/get register)]))

(key/action
  action/paste
  "Insert the contents of the default register."
  (register/insert ""))

(key/action
  action/paste-clipboard
  "Insert the contents of the system clipboard."
  (register/insert "+"))
