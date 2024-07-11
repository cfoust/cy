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

(test "rset root param"
      (param/rset :test 3)
      (assert (= 3 (param/get :test))))

(test "nil if doesn't exist"
      (assert (= nil (param/get :foo))))

# TODO(cfoust): 07/11/24 (param/get :target) tests
# TODO(cfoust): 07/11/24 cascade
# TODO(cfoust): 07/11/24 free existing Janet value

