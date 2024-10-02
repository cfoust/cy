(test "(color-maps/get)"
      (assert (not (nil? (color-maps/get :google-dark)))))

(test "(color-maps/set)"
      (color-maps/set :root :google-dark))
