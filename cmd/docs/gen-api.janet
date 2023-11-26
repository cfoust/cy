(defn- all-entries
  [&opt env]
  (default env root-env)
  (sort (pairs env)))

(var envs @[])

(var env (cy/env))
(while (not= nil env)
  (array/push envs (seq [x :in (all-entries env)
                         :let [[k entry] x]
                         :when (symbol? k)
                         :when (get entry :doc)
                         :when (not (get entry :private))]
                     x))
  (set env (getproto env)))

# Remove cy/doc and cy/env
(set envs (array/slice envs 2 -2))

(var symbols @[])
(each env envs
  (array/concat symbols env))

# Format and send all the symbols upwards
(each [name {:macro macro :doc doc :source-map [file line]}] symbols
  (default macro false)
  (cy/doc
    (string name)
    doc
    (if (> (length file) 0)
      (string
        "https://github.com/cfoust/cy/blob/main/pkg/cy/cy-boot.janet#L"
        line)
      "")
    macro))
