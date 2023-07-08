(try
  (do
    (key/bind ["ctrl+a"] "quit" (fn [&] (log "hello"))))
  ([err] (log (string "caught error: " err))))
