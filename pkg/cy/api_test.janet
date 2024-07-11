# This is the standard library for all API tests.

(defmacro test
  [name & body]
  ~(upscope
     (,run-test ,name (fn [] ,;body))))

(defmacro expect-error
  [& body]
  ~(upscope
     (try
       (do
         ,;body
         (error "should have errored"))
       ([err fib]))))
