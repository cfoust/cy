(defn
  layout/type?
  ```Report whether node is of the provided type.```
  [type node]
  (= (node :type) type))

(defn
  layout/pane?
  ```Report whether node is of type :pane.```
  [node]
  (layout/type? :pane node))

(defn
  layout/find-path
  ```Get the path to the first node satisfying the predicate function or nil if none exists.```
  [node predicate]
  (if (predicate node) (break @[]))

  (cond
    (layout/type? :split node) (do
                                 (def {:a a :b b} node)
                                 (def a-path (layout/find-path a predicate))
                                 (if (not (nil? a-path)) (break @[:a ;a-path]))
                                 (def b-path (layout/find-path b predicate))
                                 (if (not (nil? b-path)) (break @[:b ;b-path]))
                                 nil)
    (layout/type? :margins node) (do
                                   (def {:node node} node)
                                   (def path (layout/find-path node predicate))
                                   (if (not (nil? path)) (break @[:node ;path])))
    nil))

(defn
  layout/has?
  ```Report whether this node or one of its descendants matches the predicate function.```
  [node predicate]
  (not (nil? (layout/find-path node predicate))))

(defn
  layout/attached?
  ```Report whether node or one of its descendants is attached.```
  [node]
  (not (nil? (layout/find-path node |($ :attached)))))

(defn
  layout/attach-path
  ```Get the path to the attached node for the given node.```
  [node]
  (layout/find-path node |($ :attached)))

(defn
  layout/path
  ```Resolve the path to a node. Returns nil if any portion of the path is invalid.```
  [node path]
  (if (= (length path) 0) (break node))
  (def [head & rest] path)
  (if (nil? (node head)) (break nil))
  (layout/path (node head) rest))

(defn
  layout/assoc
  ```Set the node at the given path in layout to the provided node. Returns a copy of the original layout with the node changed.```
  [layout path node]
  (if (= (length path) 0) (break node))
  (def [head & rest] path)
  (if (nil? (layout head)) (break layout))
  (def new-layout (struct/to-table layout))
  (put new-layout head (layout/assoc (layout head) rest node))
  (table/to-struct new-layout))

(defn
  layout/get-last
  ```Get the path to the last node in the path where (predicate node) evaluates to true.```
  [layout path predicate]
  # Must be a valid path and actually map to a node
  (if (= (length path) 0) (break nil))
  (if (nil? (layout/path layout path)) (break nil))

  (def found-path
    (find
      |(predicate (layout/path layout (array/slice path ;$)))
      (->>
        (range (length path))
        (map |(tuple 0 $))
        (reverse))))

  (if (nil? found-path) (break nil))
  (array/slice path ;found-path))

(defn
  layout/replace
  ```Replace the node at the path by passing it through a replacer function.```
  [node path replacer]
  (layout/assoc node path (replacer (layout/path node path))))

(defn
  layout/replace-attached
  ```Replace the attached pane in this tree with a new one using the provided replacer function. This function will be invoked with a single argument, the node that is currently attached, and it should return a new node.```
  [node replacer]
  (if (layout/pane? node) (break (replacer node)))
  (def path (layout/attach-path node))
  (if (nil? path) (break node))
  (def current (layout/path node path))
  (layout/assoc node path (replacer current)))

(defn
  layout/detach
  ```Detach the attached node in the tree.```
  [node]
  (layout/replace-attached node |(do {:type :pane :id ($ :id)})))

(defn
  layout/attach
  ```Attach to the node at path in layout.```
  [layout path]
  (layout/replace
    (layout/detach layout)
    path
    |{:type :pane :id ($ :id) :attached true}))

(defn
  layout/find-bottom
  ```Find the path to the node at the bottom.```
  [node]
  (if (layout/pane? node) (break @[]))

  (cond
    (layout/type? :split node) (do
                                 (def {:vertical vertical :a a :b b} node)
                                 (if vertical
                                   @[:b ;(layout/find-bottom b)]
                                   @[:a ;(layout/find-bottom a)]))
    (layout/type? :margins node) @[:node ;(layout/find-bottom (node :node))]
    nil))

(defn
  layout/move
  ```This function attaches to the pane nearest to the one the user is currently attached to along an axis. It returns a new copy of layout with the attachment point changed or returns the same layout if no motion could be completed.

is-axis is a unary function that, given a node, returns a boolean that indicates whether the node is arranged _along the axis in question._ For example, when moving vertically, a vertical split (two panes on top of each other) would return true.

successors is a unary function that, given a node, returns the paths of all of the child nodes accessible from the node in the order of their appearance along the axis. For example, when moving vertically upwards, for a vertical split node this function would return @[[:b] [:a]], because :b is the first node from the bottom, and when moving vertically downwards it would return @[[:a] [:b]] because :a is the first node from the top.
  ```
  [layout is-axis successors]
  (def path (layout/attach-path layout))
  (if (nil? path) (break layout))

  # We look for a path we can attach to in the opposite direction of
  # movement.
  # 
  # Consider the case where a node has successors :a, :b:, and :c arranged
  # along the axis of motion; if we're attached to a node on :b and moving in
  # the direction of :a, we want `detached-successors` to return just [:a],
  # since [:c] is "after" or "below" us.
  (defn detached-successors [node]
    (->>
      (successors node)
      (reverse)
      (take-while |(not (layout/attached? (layout/path node $))))
      (reverse)))

  (defn check-node [node]
    (and (is-axis node) (> (length (detached-successors node)) 0)))

  # We first find the most recent ancestor to the node we're attached to that
  # has a child tree that we can move to.
  (def branch-path (layout/get-last layout path check-node))
  (if (nil? branch-path) (break layout))

  (def [next-path] (detached-successors (layout/path layout branch-path)))
  (def full-path @[;branch-path ;next-path])

  # Find the closest pane we can attach to in the direction of motion.
  (defn
    find-nearest
    [node]
    (if (layout/pane? node) (break @[]))
    (def [nearest] (successors node))
    @[;nearest ;(find-nearest (layout/path node nearest))])

  (layout/attach
    layout
    @[;full-path ;(find-nearest (layout/path layout full-path))]))

(defn
  layout/move-up
  ```Change the layout by moving to the next node "above" the attached pane.```
  [layout]
  (layout/move
    layout
    |(and (layout/type? :split $) ($ :vertical))
    |(cond
       (layout/type? :split $) (if ($ :vertical)
                                 @[[:b] [:a]]
                                 @[[:a] [:b]])
       (layout/type? :margins $) @[[:node]]
       @[[]])))

(defn
  layout/move-down
  ```Change the layout by moving to the next node "below" the attached pane.```
  [layout]
  (layout/move
    layout
    |(and (layout/type? :split $) ($ :vertical))
    |(cond
       (layout/type? :split $) @[[:a] [:b]]
       (layout/type? :margins $) @[[:node]]
       @[[]])))

(defn
  layout/split-right
  ```Split the currently attached pane into two horizontally, replacing the right pane with the given node.```
  [layout node]
  (layout/replace-attached layout |(do {:type :split
                                        :percent 50
                                        :a (layout/detach $)
                                        :b node})))

(defn
  layout/split-left
  ```Split the currently attached pane into two horizontally, replacing the left pane with the given node.```
  [layout node]
  (layout/replace-attached layout |(do {:type :split
                                        :percent 50
                                        :b (layout/detach $)
                                        :a node})))

(defn
  layout/split-down
  ```Split the currently attached pane into two vertically, replacing the bottom pane with the given node.```
  [layout node]
  (layout/replace-attached layout |(do {:type :split
                                        :percent 50
                                        :vertical true
                                        :a (layout/detach $)
                                        :b node})))

(defn
  layout/split-up
  ```Split the currently attached pane into two vertically, replacing the top pane with the given node.```
  [layout node]
  (layout/replace-attached layout |(do {:type :split
                                        :percent 50
                                        :vertical true
                                        :b (layout/detach $)
                                        :a node})))

# TODO(cfoust): 07/25/24 clean this up
(key/action
  action/split-right
  "Split the current pane to the right."
  (def path (cmd/path (pane/current)))
  (def shells (group/mkdir :root "/shells"))
  (def shell (cmd/new shells :path path :name (path/base path)))
  (layout/set
    (layout/split-right
      (layout/get)
      {:type :pane :id shell :attached true})))

(key/action
  action/split-right
  "Split the current pane to the left"
  (def path (cmd/path (pane/current)))
  (def shells (group/mkdir :root "/shells"))
  (def shell (cmd/new shells :path path :name (path/base path)))
  (layout/set
    (layout/split-left
      (layout/get)
      {:type :pane :id shell :attached true})))

(key/action
  action/split-down
  "Split the current pane downwards."
  (def path (cmd/path (pane/current)))
  (def shells (group/mkdir :root "/shells"))
  (def shell (cmd/new shells :path path :name (path/base path)))
  (layout/set
    (layout/split-down
      (layout/get)
      {:type :pane :id shell :attached true})))

(key/action
  action/split-up
  "Split the current pane upwards"
  (def path (cmd/path (pane/current)))
  (def shells (group/mkdir :root "/shells"))
  (def shell (cmd/new shells :path path :name (path/base path)))
  (layout/set
    (layout/split-up
      (layout/get)
      {:type :pane :id shell :attached true})))

(key/action
  action/move-up
  "Move up to the next pane."
  (layout/set (layout/move-up (layout/get))))

(key/action
  action/move-down
  "Move down to the next pane."
  (layout/set (layout/move-down (layout/get))))
