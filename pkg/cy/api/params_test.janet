(test "no keyword"
      (expect-error (param/set :root 2 2)))

(test "invalid parameter"
      (expect-error (param/set :root :data-directory 2)))

(test-no-context "missing client"
                 (expect-error (param/set :client :blah 2)))

(test "client param"
      (param/set :client :test 2)
      (assert (= 2 (param/get :test))))

(test "root param"
      (param/set :root :test 2)
      (assert (= 2 (param/get :test))))

(test "cascade"
      (param/set :root :test 4)
      (def group (group/new :root))
      (assert (= 4 (param/get :test :target group)))
      (param/set group :test 5)
      (assert (= 5 (param/get :test :target group))))

(test "rset root param"
      (param/rset :test 3)
      (assert (= 3 (param/get :test))))

(test "nil if doesn't exist"
      (assert (= nil (param/get :foo))))

(test "param/set-many"
      (param/set-many :root
                      :test 1
                      :test2 2)
      (assert (= 2 (param/get :test2))))

(test "color map"
      (param/set :root :color-map {"0" "#ff0000"})
      (assert (deep= @{"0" "#ff0000"} (param/get :color-map))))

(test ":persist"
      (var value @{:key "value" :num 123 :ok true})
      (param/set :persist :test-table value)
      (assert (deep= value (param/get :test-table :target :persist)))

      # persist a func that uses a cy API function
      (var func
        (fn [a]
          (input/text "blah")
          (+ a a)))
      (param/set :persist :test-func func)
      (var func-after (param/get :test-func :target :persist))
      (assert (= (func-after 2) 4))

      # nonexistent key
      (assert (= nil (param/get :nonexistent :target :persist))))
