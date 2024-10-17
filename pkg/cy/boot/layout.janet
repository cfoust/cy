(defn
  assoc
  "Set a property in a struct, returning a new struct."
  [s key value]
  (def new (struct/to-table s))
  (put new key value)
  (table/to-struct new))

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
  (if (not (struct? node)) (break false))
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
    (layout/type? :tabs node) (map
                                |[:tabs $ :node]
                                (->
                                  (node :tabs)
                                  (length)
                                  (range)))
    (or
      (layout/type? :margins node)
      (layout/type? :bar node)
      (layout/type? :borders node)
      (layout/type? :color-map node)) @[[:node]]
    @[]))

(defn
  layout/pane?
  ```Report whether node is of type :pane.```
  [node]
  (layout/type? :pane node))

(defn
  layout/pane
  ```Convenience function for creating a new :pane node.```
  [&named id attached remove-on-exit]
  (default attached false)
  {:type :pane
   :id id
   :attached attached
   :remove-on-exit remove-on-exit})

(defn
  layout/split
  ```Convenience function for creating a new :split node.```
  [a b &named vertical cells percent border border-fg border-bg]
  (default vertical false)
  {:type :split
   :a a
   :b b
   :vertical vertical
   :cells cells
   :percent percent
   :border border
   :border-fg border-fg
   :border-bg border-bg})

(defn
  layout/vsplit
  ```Convenience function for creating a new vertical :split node.```
  [a b &named cells percent border border-fg border-bg]
  (layout/split a b
                :vertical true
                :cells cells
                :percent percent
                :border border
                :border-fg border-fg
                :border-bg border-bg))

(defn
  layout/hsplit
  ```Convenience function for creating a new horizontal :split node.```
  [a b &named cells percent border border-fg border-bg]
  (layout/split a b
                :vertical false
                :cells cells
                :percent percent
                :border border
                :border-fg border-fg
                :border-bg border-bg))

(defn
  layout/margins
  ```Convenience function for creating a new :margins node.```
  [node &named cols rows border border-fg border-bg]
  {:type :margins
   :node node
   :cols cols
   :rows rows
   :border border
   :border-fg border-fg
   :border-bg border-bg})

(defn
  layout/borders
  ```Convenience function for creating a new :borders node.```
  [node &named title title-bottom border border-fg border-bg]
  {:type :borders
   :node node
   :title title
   :title-bottom title-bottom
   :border border
   :border-fg border-fg
   :border-bg border-bg})

(defn
  layout/tab
  ```Convenience function for creating a new tab (inside of a :tabs node).```
  [name
   node
   &named
   active]
  {:name name
   :node node
   :active active})

(defn
  layout/tabs
  ```Convenience function for creating a new :tabs node.```
  [tabs
   &named
   active-fg
   active-bg
   inactive-fg
   inactive-bg
   bg
   bottom]
  {:type :tabs
   :tabs tabs
   :active-fg active-fg
   :active-bg active-bg
   :inactive-fg inactive-fg
   :inactive-bg inactive-bg
   :bg bg
   :bottom bottom})

(defn
  layout/bar
  ```Convenience function for creating a new :bar node.```
  [text node &named bottom]
  {:type :bar
   :node node
   :text text
   :bottom bottom})

(defn
  layout/color-map
  ```Convenience function for creating a new :color-map node.```
  [map node]
  {:type :color-map
   :map map
   :node node})

(defmacro
  layout/new
  ```Macro for quickly creating layouts. layout/new replaces shorthand versions of node creation functions with their longform versions and also includes a few abbreviations that do not exist elsewhere in the API.

Supported short forms:
* pane: A detached :pane node.
* attach: An attached :pane node.
* split: A :split node.
* hsplit: A :split node with :vertical=false.
* vsplit: A :split node with :vertical=true.
* borders: A :borders node.
* margins: A :margins node.
* tabs: A :tabs node.
* tab: A :tab inside of a :tabs node.
* active-tab: A :tab with :active=true inside of a :tabs node.
* bar: A :bar node.
* color-map: A :color-map node.

See [the layouts chapter](/layouts.md#api) for more information.
  ```
  [body]
  (defn attach
    [&named id remove-on-exit]
    (layout/pane :id id
                 :remove-on-exit remove-on-exit
                 :attached true))
  (defn active-tab
    [name node]
    (layout/tab name node :active true))

  ~(do
     (def pane ,layout/pane)
     (def attach ,attach)
     (def split ,layout/split)
     (def hsplit ,layout/hsplit)
     (def vsplit ,layout/vsplit)
     (def borders ,layout/borders)
     (def margins ,layout/margins)
     (def tabs ,layout/tabs)
     (def tab ,layout/tab)
     (def active-tab ,active-tab)
     (def bar ,layout/bar)
     (def color-map ,layout/color-map)
     ,body))

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
  (if (nil? path) (break nil))
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
  (if (= (length found-path) 0) (break @[]))
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
  layout/attach-id
  ```Get the NodeID of the attached node for the given node.```
  [node]
  (def path (layout/find node |($ :attached)))
  (if (nil? path) (break))
  ((layout/path node path) :id))

(defn
  layout/assoc
  ```Set the node at the given path in layout to the provided node. Returns a copy of the original layout with the node changed.```
  [layout path node]
  (if (= (length path) 0) (break node))
  (def [head & rest] path)
  (if (nil? (layout head)) (break layout))
  (if (and (number? head) (not (indexed? layout))) (break layout))
  (if (number? head)
    (break (->>
             (pairs layout)
             (map |(if (= head ($ 0))
                     (layout/assoc (layout head) rest node)
                     ($ 1))))))

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
  (layout/replace-attached node |(layout/pane :id ($ :id))))

(defn
  layout/attach
  ```Attach to the node at path in layout.```
  [layout path]
  (layout/replace
    (layout/detach layout)
    path
    |(layout/pane :attached true :id ($ :id))))

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
  (layout/replace-attached layout |(layout/split (layout/detach $) node)))

(defn
  layout/split-left
  ```Split the currently attached pane into two horizontally, replacing the left pane with the given node.```
  [layout node]
  (layout/replace-attached layout |(layout/split node (layout/detach $))))

(defn
  layout/split-down
  ```Split the currently attached pane into two vertically, replacing the bottom pane with the given node.```
  [layout node]
  (layout/replace-attached layout |(do {:type :split
                                        :vertical true
                                        :a (layout/detach $)
                                        :b node})))

(defn
  layout/split-up
  ```Split the currently attached pane into two vertically, replacing the top pane with the given node.```
  [layout node]
  (layout/replace-attached layout |(do {:type :split
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

  (def new-parent
    (cond
      (layout/type?
        :tabs
        parent) (do
                  (def [head & rest] (filter
                                       |(not (layout/attached? ($ :node)))
                                       (parent :tabs)))

                  (def new-head
                    (as?-> head _
                           (assoc _ :node (layout/attach-first (_ :node)))
                           (assoc _ :active true)))

                  (assoc parent :tabs @[new-head ;rest]))
      (layout/type?
        :split
        parent) (do
                  (def {:a a :b b} parent)
                  (cond
                    (layout/attached? a) (layout/attach-first b)
                    (layout/attached? b) (layout/attach-first a)))))

  (layout/assoc layout parent-path new-parent))

(key/action
  action/remove-layout-pane
  "Remove the current pane from the layout."
  (layout/set (layout/remove-attached (layout/get))))

(key/action
  action/kill-layout-pane
  "Remove the current pane from the layout and the node tree."
  (def layout (layout/get))
  (def {:id id} (layout/path layout (layout/attach-path layout)))
  (layout/set (layout/remove-attached layout))
  (if (not (nil? id)) (tree/rm id)))

(key/action
  action/kill-pane
  "Kill the process of the current pane, but do not detach from it."
  (cmd/kill (pane/current)))

(key/action
  action/kill-and-reattach
  `Remove the current pane from the node tree and attach to a new one.

  A new shell will be created if the current pane is the last one in the layout.`
  (def id (pane/current))
  (if (nil? id) (break))
  (def head (or
              (find |(and
                       (not (= id $))
                       (not (= (tree/path $) "/logs"))) (group/leaves :root))
              (shell/new)))
  (pane/attach head)
  (tree/rm id))

(defmacro-
  pane-creator
  [name docstring transformer]
  ~(upscope
     (key/action
       ,name
       ,docstring
       (def [ok path] (protect (cmd/path (pane/current))))
       (def shells (,group/mkdir :root "/shells"))
       (def shell (,cmd/new shells
                            :path (if ok path nil)
                            :name (,path/base path)))
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
  action/add-node
  "Add a node to the layout."
  (def layout (layout/get))
  (def types @[[":borders" |(struct :type :borders :node $)]
               [":margins" |(struct :type :margins :node $)]
               [":tabs" |(layout/new
                           (tabs
                             [(active-tab "new tab" $)]))]
               [":tabs (bottom)" |(layout/new
                                    (tabs
                                      [(active-tab "new tab" $)]
                                      :bottom true))]
               [":split (horizontal)" |(struct :type :split

                                               :a $
                                               :b {:type :pane})]
               [":split (vertical)" |(struct :type :split
                                             :vertical true
                                             :a $
                                             :b {:type :pane})]])
  (as?-> types _
         (map (fn [[name mapper]]
                (def new-layout (layout/replace-attached layout mapper))
                [name {:type :layout :layout new-layout} new-layout])
              _)
         (input/find _ :prompt "add a node" :animate false)
         (layout/set _)))

(key/action
  action/remove-parent
  "Remove the parent of the current node."
  (def layout (layout/get))
  (def path (layout/attach-path layout))
  (if (< (length path) 1) (break))
  (layout/set (layout/assoc layout
                            (array/slice path 0 (- (length path) 1))
                            (layout/path layout path))))

(defn-
  set-borders-property
  [prop message]
  (def layout (layout/get))
  (def path (layout/attach-path layout))
  (def borders-path (layout/find-last layout path |(layout/type? :borders $)))
  (if (nil? borders-path) (break))
  (def borders (struct/to-table (layout/path layout borders-path)))
  (var old (get borders prop))
  (default old "")
  (def new (input/text message :preset old :animated false))
  (if (nil? new) (break))
  (put borders prop new)
  (layout/set (layout/assoc layout borders-path (table/to-struct borders))))

(key/action
  action/set-borders-title
  "Set the :title for a :borders node."
  (set-borders-property :title "enter a value for :title"))

(key/action
  action/set-borders-title-bottom
  "Set the :title-bottom for a :borders node."
  (set-borders-property :title-bottom "enter a value for :title-bottom"))

(key/action
  action/rename-tab
  "Rename the current tab."
  (def layout (layout/get))
  (def path (layout/attach-path layout))
  (def tabs-path (layout/find-last layout path |(layout/type? :tabs $)))
  (if (nil? tabs-path) (break))
  (def tabs (layout/path layout tabs-path))
  (def active-path (find
                     |((layout/path tabs (array/slice $ 0 2)) :active)
                     (layout/successors tabs)))

  (def tab-path (array/slice active-path 0 2))
  (def active (layout/path tabs tab-path))
  (def new-name (input/text
                  "set the tab name"
                  :preset (active :name)
                  :animated false))

  (if (or (nil? new-name) (= 0 (length new-name))) (break))

  (def new-layout
    (layout/assoc
      layout
      @[;tabs-path ;tab-path]
      (assoc active :name new-name)))

  (layout/set new-layout))

(key/action
  action/clear-layout
  "Clear out the layout."
  (layout/set {:type :pane :attached true}))

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

(key/action
  action/new-tab
  "Create a new tab."
  (def layout (layout/get))
  (def shell (shell/new))
  (def new-tab (layout/new
                 (tab "new tab" (attach :id shell) :active true)))

  (def detached (layout/detach layout))

  (def tabs-path (layout/find-last
                   layout
                   (layout/attach-path layout)
                   |(layout/type? :tabs $)))

  (def new-layout (if (nil? tabs-path)
                    (layout/new
                      (tabs
                        @[(tab "new tab" detached)
                          new-tab]))

                    (do
                      (def node (layout/path detached tabs-path))
                      (def {:tabs existing-tabs} node)
                      (layout/assoc
                        detached
                        tabs-path
                        (assoc node :tabs
                               @[;(map |(assoc $ :active false) existing-tabs)
                                 new-tab])))))

  (layout/set new-layout))

(defn-
  switch-tab-delta
  [delta]
  (def layout (layout/get))
  (def tabs-path (layout/find-last
                   layout
                   (layout/attach-path layout)
                   |(layout/type? :tabs $)))
  (if (nil? tabs-path) (break))

  (def detached (layout/detach layout))
  (def node (layout/path detached tabs-path))
  (def [_ active-index] (find
                          |((layout/path node (array/slice $ 0 2)) :active)
                          (layout/successors node)))

  (def {:tabs existing-tabs} node)
  (def new-index (mod (+ active-index delta) (length existing-tabs)))

  (def new-layout
    (layout/assoc
      detached
      tabs-path
      (assoc node :tabs
             (->>
               (pairs existing-tabs)
               (map |(if (= new-index ($ 0))
                       (as?-> ($ 1) _
                              (assoc _ :active true)
                              (assoc _ :node (layout/attach-first (_ :node))))
                       (assoc ($ 1) :active false)))))))

  (layout/set new-layout))

(key/action
  action/next-tab
  "Switch to the next tab."
  (switch-tab-delta 1))

(key/action
  action/prev-tab
  "Switch to the previous tab."
  (switch-tab-delta -1))

(merge-module root-env (curenv))
