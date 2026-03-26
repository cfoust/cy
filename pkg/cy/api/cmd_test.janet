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

(test "(cmd/wait) basic"
  (def pane (cmd/new :root :command "echo" :args @["hello world"] :temp true))
  (def result (cmd/wait pane))
  (assert (= (result :exit-code) 0))
  (assert (string/find "hello world" (result :output))))

(test "(cmd/wait) exit code"
  (def pane (cmd/new :root :command "sh" :args @["-c" "exit 42"] :temp true))
  (def result (cmd/wait pane))
  (assert (= (result :exit-code) 42)))

(test "(cmd/wait) rejects restart pane"
  (def pane (cmd/new :root :command "true" :restart true))
  (assert (= :error
             (try (do (cmd/wait pane) :ok)
               ([err] :error))))
  (cmd/kill pane))

(test "(cmd/wait) remove"
  (def pane (cmd/new :root :command "true" :temp true))
  (def result (cmd/wait pane :remove true))
  (assert (= (result :exit-code) 0)))
