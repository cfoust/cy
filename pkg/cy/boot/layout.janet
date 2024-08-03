(defn
  layout/path
  ```Resolve the path to a node. Returns nil if any portion of the path is invalid.```
  [node path]
  (if (= (length path) 0) (break node))
  (def [head & rest] path)
  (if (nil? (node head)) (break nil))
  (layout/path (node head) rest))

(defn
  layout/type?
  ```Report whether node is of the provided type.```
  [type node]
  (= (node :type) type))

(defn
  layout/successors
  ````Get the paths to all of the direct children of this node.

For example:
```janet
# For a node of type :split
@[[:a] [:b]]
# For a node of type :pane (it has no children)
@[]
```
  ````
  [node]
  (cond
    (layout/type? :split node) @[[:a] [:b]]
    (or
      (layout/type? :margins node)
      (layout/type? :borders node)) @[[:node]]
    @[]))

(defn
  layout/pane?
  ```Report whether node is of type :pane.```
  [node]
  (layout/type? :pane node))

(defn
  layout/find
  ```Get the path to the first node satisfying the predicate function or nil if none exists.```
  [node predicate]
  (if (predicate node) (break @[]))
  (var path nil)

  (each successor (layout/successors node)
    (def found (layout/find
                 (layout/path node successor)
                 predicate))
    (if
      (not (nil? found))
      (set path @[;successor ;found])))

  path)

(defn
  layout/find-last
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
  layout/has?
  ```Report whether this node or one of its descendants matches the predicate function.```
  [node predicate]
  (not (nil? (layout/find node predicate))))

(defn
  layout/attached?
  ```Report whether node or one of its descendants is attached.```
  [node]
  (not (nil? (layout/find node |($ :attached)))))

(defn
  layout/attach-path
  ```Get the path to the attached node for the given node.```
  [node]
  (layout/find node |($ :attached)))

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
  layout/move
  ```This function attaches to the pane nearest to the one the user is currently attached to along an axis. It returns a new copy of layout with the attachment point changed or returns the same layout if no motion could be completed.

is-axis is a unary function that, given a node, returns a boolean that indicates whether the node is arranged _along the axis in question._ For example, when moving vertically, a vertical split (two panes on top of each other) would return true.

successors is a unary function that, given a node where is-axis was true, returns the paths of all of the child nodes accessible from the node in the order of their appearance along the axis.

For example, when moving vertically upwards, for a vertical split node this function would return @[[:b] [:a]], because :b is the first node from the bottom, and when moving vertically downwards it would return @[[:a] [:b]] because :a is the first node from the top.
  ```
  [layout is-axis axis-successors]
  (def path (layout/attach-path layout))
  (if (nil? path) (break layout))

  (defn successors
    [node]
    (if (is-axis node)
      (axis-successors node)
      (layout/successors node)))

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
  (def branch-path (layout/find-last layout path check-node))
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
    |(identity (reverse (layout/successors $)))))

(defn
  layout/move-down
  ```Change the layout by moving to the next node "below" the attached pane.```
  [layout]
  (layout/move
    layout
    |(and (layout/type? :split $) ($ :vertical))
    |(identity (layout/successors $))))

(defn
  layout/move-left
  ```Change the layout by moving to the next node to the left of the attached pane.```
  [layout]
  (layout/move
    layout
    |(and (layout/type? :split $) (not ($ :vertical)))
    |(identity (reverse (layout/successors $)))))

(defn
  layout/move-right
  ```Change the layout by moving to the next node to the right of the attached pane.```
  [layout]
  (layout/move
    layout
    |(and (layout/type? :split $) (not ($ :vertical)))
    |(identity (layout/successors $))))

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

(defn
  layout/attach-first
  ```Attach to the first pane found in the layout.```
  [layout]
  (def path (layout/find layout layout/pane?))
  (if (nil? path) (break layout))
  (layout/attach layout path))

(defn
  layout/map
  ```Pass all nodes in the tree into a mapping function.```
  [mapping layout]
  (def mapped (mapping layout))
  (reduce
    (fn [node successor]
      (layout/assoc node successor
                    (layout/map mapping (layout/path node successor))))
    mapped
    (layout/successors mapped)))

(defn
  layout/remove-attached
  ```Remove the attached node from the layout, simplifying the nearest ancestor with children.```
  [layout]
  (def path (layout/attach-path layout))
  (if (nil? path) (break layout))
  (def parent-path (layout/find-last
                     layout
                     path
                     |(> (length (layout/successors $)) 1)))

  # If there are no parents with other children, it's game over, just set the
  # layout to a disconnected pane
  (if (nil? parent-path)
    (break {:type :pane :attached true}))

  (def parent (layout/path layout parent-path))

  # For now, only splits satisfy this (they are the only node type that can
  # have more than one successor)
  (def {:a a :b b} parent)
  (layout/assoc
    layout
    parent-path
    (cond
      (layout/attached? a) (layout/attach-first b)
      (layout/attached? b) (layout/attach-first a))))

(key/action
  action/remove-current-pane
  "Remove the current pane from the layout."
  (layout/set (layout/remove-attached (layout/get))))

(key/action
  action/kill-current-pane
  "Kill the pane and remove it from the layout."
  (def layout (layout/get))
  (def {:id id} (layout/path layout (layout/attach-path layout)))
  (layout/set (layout/remove-attached layout))
  (if (not (nil? id)) (tree/kill id)))

(defmacro-
  pane-creator
  [name docstring transformer]
  ~(upscope
     (key/action
       ,name
       ,docstring
       (def path (,cmd/path (,pane/current)))
       (def shells (,group/mkdir :root "/shells"))
       (def shell (,cmd/new shells :path path :name (,path/base path)))
       (,layout/set
         (,transformer
           (,layout/get)
           {:type :pane :id shell :attached true})))))

(pane-creator
  action/split-right
  "Split the current pane to the right."
  layout/split-right)

(pane-creator
  action/split-left
  "Split the current pane to the left."
  layout/split-left)

(pane-creator
  action/split-up
  "Split the current pane upwards."
  layout/split-up)

(pane-creator
  action/split-down
  "Split the current pane downwards."
  layout/split-down)

(key/action
  action/move-up
  "Move up to the next pane."
  (layout/set (layout/move-up (layout/get))))

(key/action
  action/move-down
  "Move down to the next pane."
  (layout/set (layout/move-down (layout/get))))

(key/action
  action/move-left
  "Move left to the next pane."
  (layout/set (layout/move-left (layout/get))))

(key/action
  action/move-right
  "Move right to the next pane."
  (layout/set (layout/move-right (layout/get))))

(key/action
  action/add-borders
  "Add borders to the current pane."
  (layout/set
    (layout/replace-attached
      (layout/get)
      |(struct :type :borders :node $))))

(key/action
  action/toggle-margins
  "Toggle the screen's margins."
  (def layout (layout/get))
  (def path (layout/attach-path layout))
  (def margins-path (layout/find-last layout path |(layout/type? :margins $)))
  (layout/set (if (not (nil? margins-path))
                (do
                  (def {:node node} (layout/path layout margins-path))
                  (layout/assoc layout margins-path node))
                {:type :margins :node layout :cols 80 :rows 0})))

(key/action
  action/margins-80
  "Toggle the screen's margins."
  (def layout (layout/get))
  (def path (layout/attach-path layout))
  (def margins-path (layout/find-last layout path |(layout/type? :margins $)))
  (layout/set (if (not (nil? margins-path))
                (do
                  (def {:node node} (layout/path layout margins-path))
                  (layout/assoc layout margins-path node))
                {:type :margins :node layout :cols 80 :rows 0})))

(key/action
  action/margins-80
  "Set margins size to 80 columns."
  (def layout (layout/get))
  (def path (layout/attach-path layout))
  (def margins-path (layout/find-last layout path |(layout/type? :margins $)))
  (layout/set (if (not (nil? margins-path))
                (do
                  (def {:node node} (layout/path layout margins-path))
                  (layout/assoc
                    layout
                    margins-path
                    {:type :margins :node node :cols 80 :rows 0}))
                {:type :margins :node layout :cols 80 :rows 0})))

(key/action
  action/margins-160
  "Set size to 160 columns."
  (def layout (layout/get))
  (def path (layout/attach-path layout))
  (def margins-path (layout/find-last layout path |(layout/type? :margins $)))
  (layout/set (if (not (nil? margins-path))
                (do
                  (def {:node node} (layout/path layout margins-path))
                  (layout/assoc
                    layout
                    margins-path
                    {:type :margins :node node :cols 160 :rows 0}))
                {:type :margins :node layout :cols 160 :rows 0})))

(key/action
  action/margins-smaller
  "Decrease margins by 5 columns."
  (def layout (layout/get))
  (def path (layout/attach-path layout))
  (def margins-path (layout/find-last layout path |(layout/type? :margins $)))
  (if (nil? margins-path) (break))
  (def {:node node :cols cols :rows rows} (layout/path layout margins-path))
  (layout/set (layout/assoc
                layout
                margins-path
                {:type :margins
                 :node node
                 :cols (max (- cols 5) 5)
                 :rows rows})))

(key/action
  action/margins-bigger
  "Increase margins by 5 columns."
  (def layout (layout/get))
  (def path (layout/attach-path layout))
  (def margins-path (layout/find-last layout path |(layout/type? :margins $)))
  (if (nil? margins-path) (break))
  (def {:node node :cols cols :rows rows} (layout/path layout margins-path))
  (layout/set (layout/assoc
                layout
                margins-path
                {:type :margins
                 :node node
                 :cols (+ cols 5)
                 :rows rows})))


(key/action
  action/set-layout-borders
  "Change the border style across the entire layout."
  (def layout (layout/get))
  (as?-> (map (fn [style]
                (def style-layout (layout/map
                                    (fn [node]
                                      (def {:border border} node)
                                      (if (or
                                            (= border :none)
                                            (nil? border)) (break node))
                                      (def modifiable (struct/to-table node))
                                      (put modifiable :border style)
                                      (table/to-struct modifiable)) layout))
                [(string style)
                 {:type :layout :layout style-layout}
                 style-layout])
              @[:normal
                :rounded
                :block
                :outer-half
                :inner-half
                :thick
                :double
                :hidden]) _
         (input/find _
                     :prompt "choose a border style")
         (layout/set _)))
