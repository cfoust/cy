# This is the standard library for all API tests.

(defmacro test
  "Run a test with a default-sized client."
  [name & body]
  ~(upscope
     (,run-test ,name true (fn [] ,;body))))

(defmacro test-no-context
  "Run a test without attaching a client."
  [name & body]
  ~(upscope
     (,run-test ,name false (fn [] ,;body))))

(defmacro expect-error
  [& body]
  ~(upscope
     (try
       (do
         ,;body
         (error "should have errored"))
       ([err fib]))))
