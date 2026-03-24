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

(test "(cmd/exec) basic command"
  (def result (cmd/exec ["echo" "hello"]))
  (assert (= (result :stdout) "hello\n"))
  (assert (= (result :stderr) ""))
  (assert (= (result :exit-code) 0)))

(test "(cmd/exec) with arguments"
  (def result (cmd/exec ["printf" "%s %s" "foo" "bar"]))
  (assert (= (result :stdout) "foo bar"))
  (assert (= (result :exit-code) 0)))

(test "(cmd/exec) with stdin"
  (def result (cmd/exec ["cat"] :stdin "hello world"))
  (assert (= (result :stdout) "hello world"))
  (assert (= (result :exit-code) 0)))

(test "(cmd/exec) non-zero exit code"
  (def result (cmd/exec ["false"]))
  (assert (not= (result :exit-code) 0)))

(test "(cmd/exec) with env"
  (def result (cmd/exec ["sh" "-c" "echo $TEST_VAR"] :env {"TEST_VAR" "test_value"}))
  (assert (= (result :stdout) "test_value\n"))
  (assert (= (result :exit-code) 0)))

(test "(cmd/exec) captures stderr"
  (def result (cmd/exec ["sh" "-c" "echo error >&2"]))
  (assert (= (result :stderr) "error\n"))
  (assert (= (result :exit-code) 0)))
