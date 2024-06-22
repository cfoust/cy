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

(var lookup @{})
# Format and send all the symbols upwards
(each [name {:macro macro
             :doc doc
             :source-map [file line]
             :value value}] symbols
  (put lookup value name)
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

(defn handle-binding [source binding]
  (def {:sequence sequence :function function} binding)
  (def func (get lookup function))
  (default func "")
  (cy/bind source sequence (string func)))

(each binding (key/get :root) (handle-binding "root" binding))
(each binding (key/get :time) (handle-binding "time" binding))
(each binding (key/get :copy) (handle-binding "copy" binding))
