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
