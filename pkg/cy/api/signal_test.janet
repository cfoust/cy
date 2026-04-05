(test "(signal/send) without waiters does not error"
  (signal/send :no-one-listening "hello"))

(test "(signal/send) with nil value"
  (signal/send :nil-test nil))

(test "(signal/wait) timeout"
  (assert (= :error
             (try (do (signal/wait :never-signaled :timeout 0.1) :ok)
               ([err] :error)))))
