(test "(cmd/new)"
      (def cmd (cmd/new :root))
      (cmd/kill cmd))

(test "(cmd/title)"
      (def cmd (cmd/new :root))
      (try
        (do
          (def title (cmd/title cmd))
          (assert (string? title)))
        ([err] (error err)))
      (cmd/kill cmd))
