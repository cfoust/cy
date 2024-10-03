(test "(cmd/new)"
      (def cmd (cmd/new :root))
      (cmd/kill cmd))
