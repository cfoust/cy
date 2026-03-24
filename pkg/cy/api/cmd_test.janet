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

(test "(cmd/execute) basic command"
  (def result (cmd/execute ["echo" "hello"]))
  (assert (= (result :stdout) "hello\n"))
  (assert (= (result :stderr) ""))
  (assert (= (result :exit-code) 0)))

(test "(cmd/execute) with arguments"
  (def result (cmd/execute ["printf" "%s %s" "foo" "bar"]))
  (assert (= (result :stdout) "foo bar"))
  (assert (= (result :exit-code) 0)))

(test "(cmd/execute) with stdin"
  (def result (cmd/execute ["cat"] :stdin "hello world"))
  (assert (= (result :stdout) "hello world"))
  (assert (= (result :exit-code) 0)))

(test "(cmd/execute) non-zero exit code"
  (def result (cmd/execute ["false"]))
  (assert (not= (result :exit-code) 0)))

(test "(cmd/execute) with env"
  (def result (cmd/execute ["sh" "-c" "echo $TEST_VAR"] :env {"TEST_VAR" "test_value"}))
  (assert (= (result :stdout) "test_value\n"))
  (assert (= (result :exit-code) 0)))

(test "(cmd/execute) captures stderr"
  (def result (cmd/execute ["sh" "-c" "echo error >&2"]))
  (assert (= (result :stderr) "error\n"))
  (assert (= (result :exit-code) 0)))
