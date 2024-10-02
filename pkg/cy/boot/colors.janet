(defn
  color-maps/get
  "Get a color map by id."
  [id]
  (find |(= id (get $ :id)) (color-maps/get-all)))

(defn
  color-maps/set
  ```Set the :color-map parameter of the target node to the color map specified by id. Also sets the :color-map-id parameter.

  `target` is a [NodeID](/api.md#nodeid).
  ```
  [target id]
  (def scheme (color-maps/get id))
  (if (not scheme) (break))
  (param/set-many target
                  :color-map (scheme :map)
                  :color-map-id id))
(defn
  color-maps/get-id
  "Get the ID of the color map of the target node."
  [target]
  (param/get :color-map-id :target target))

(key/action
  action/set-pane-colors
  "Set the color map for the current pane."
  (def current (pane/current))
  (if (not current) (break))

  (as?-> (color-maps/get-all) _
         (map |(let [{:id id
                      :name name
                      :map scheme} $]
                 [[(string ":" id) name]
                  {:type :node
                   :id current
                   :color-map scheme}
                  id])
              _)
         (input/find _
                     :height 10
                     :full true
                     :prompt "color map: pane")
         (color-maps/set current _)))

(key/action
  action/show-color-map
  "Send a toast with the ID of the current color map."
  (def current (pane/current))
  (if (not current) (break))
  (def color-map (param/get :color-map-id :target current))
  (if (not color-map) (break))
  (msg/toast :info (string "color map: " (string ":" color-map))))
