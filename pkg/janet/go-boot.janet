(defn
  go/callback
  "Invoke a Go callback by name and return the result, but raise errors instead of returning them."
  [callback & args]
  (def [status result] (go/exec callback ;args))

  (case status
    :value result
    :error (error result)))

(defn
  go/make-callback
  "Return a function that will invoke the provided Go callback `callback`."
  [callback]
  (fn [& args] (go/callback callback ;args)))

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

(defn go/evaluate
  "Compile and evaluate a script and return its environment."
  [user-script &opt source]
  (def env (make-env root-env))

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
     :source source
     :on-parse-error on-parse-error
     :on-compile-error on-compile-error
     })

  (if (nil? err) env err))
