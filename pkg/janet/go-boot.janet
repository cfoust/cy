# Inspired by the code from https://janet.guide/embedding-janet/
(defn go/capture-stderr [f & args]
  (def buf @"")
  (with-dyns [*err* buf *err-color* false]
    (f ;args))
  (string/slice buf 0 -2))

(defn go/chunk-string [str]
  (var unread true)
  (fn [buf _]
    (when unread
      (set unread false)
      (buffer/blit buf str))))

(def- trace-format `%d: %s
   ■ %s:%d:%d
`)

(defn
  go/stacktrace
  "Return a nice-looking stacktrace for fiber `f` and err `err`. If `skip` is provided, skip the last N stack frames."
  [f &opt err skip]
  (default skip 0)
  (def stack (array/slice (debug/stack f) skip))

  (var result "")
  (when
    (truthy? err)
    (set result (string err "\n\n")))

  (def
    trace
    (as-> stack _
          (pairs _)
          (map
            |(let [[index {:name name
                           :source source
                           :source-line line
                           :source-column col}] $]

               (default name "<unknown>")

               (string/format
                 trace-format
                 (+ 1 index)
                 name
                 (if
                   (= (length source) 0)
                   "<anonymous>"
                   source)
                 line
                 col)) _)
          (string/join _ "\n")))

  (string
    (if
      (> (length err) 0)
      (string err "\n")
      "")
    trace))

(defn go/evaluate
  "Compile and evaluate a script and return its environment."
  [user-script source-env &opt source]
  (def env (make-env source-env))

  (var err nil)
  (var err-fiber nil)

  (defn on-parse-error [parser where]
    (set err (go/capture-stderr bad-parse parser where))
    (set (env :exit) true))

  (defn on-compile-error [msg fiber where line col]
    (set err (go/capture-stderr bad-compile msg nil where line col))
    (set err-fiber fiber)
    (set (env :exit) true))

  (run-context
    {:env env
     :chunks (go/chunk-string user-script)
     :on-parse-error on-parse-error
     :on-compile-error on-compile-error
     :on-status (fn [f x]
                  (when (not= (fiber/status f) :dead)
                    (set err (go/stacktrace f x))
                    (set err-fiber f)
                    (put env :exit true)))
     :source source
     :fiber-flags :dti})

  (if (nil? err) env err))

(defn
  go/callback
  "Invoke a Go callback by name and return the result, but raise errors instead of returning them."
  [& args]
  (def [status result] (yield args))

  (case status
    :value result
    :error (error (go/stacktrace (fiber/current) result 3))))
