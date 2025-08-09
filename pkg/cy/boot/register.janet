(defn
  register/get
  "Get the contents of the given register."
  [register]
  (if (= register "+")
    (clipboard/get)
    (param/get (keyword (string "register-" register)) :target :persist)))

(defn
  register/set
  "Set the contents of the given register."
  [register text]
  (if (= register "+")
    (clipboard/set text)
    (param/set :persist (keyword (string "register-" register)) text)))

(defn
  register/insert
  "Insert the contents of the given register in the current pane."
  [register]
  (pane/send-text
    (pane/current)
    (register/get register)))

(key/action
  action/paste
  "Insert the contents of the default register."
  (register/insert ""))

(key/action
  action/paste-clipboard
  "Insert the contents of the system clipboard."
  (register/insert "+"))
