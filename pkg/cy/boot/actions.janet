(def- actions @[])

(defmacro key/action
  ````Register an action. Equivalent to the Janet built-in `(defn`), but requires a docstring.

An action is just a Janet function that is registered to the cy server with a short human-readable string description. It provides a convenient method for making some functionality you use often more discoverable.

In a similar way to other modern applications, cy has a command palette (invoked by default with {{bind :root ctrl+a ctrl+p}}, see [`(action/command-palette)
`](/api.md#actioncommand-palette)) in which all registered actions will appear.
````
  [name docstring & body]
  ~(upscope
     (defn ,name ,docstring [] ,;body)
     (,array/push actions [,docstring ,name])))

(defmacro key/bind-many
  ````Bind many bindings at once in the same scope.

For example:
```janet
(key/bind-many :root
               [prefix "j"] action/new-shell
               [prefix "n"] action/new-project)
```
````
  [scope & body]

  (when (not (= (% (length body) 2) 0))
    (error "key/bind-many requires an even number of arguments"))

  (as?-> body _
         (length _)
         (range 0 _ 2)
         (map |(tuple ;(array/slice body $ (+ $ 2))) _)
         (map |(do
                 (def [binding func] $)
                 (tuple 'key/bind scope binding func)) _)))

(defmacro key/bind-many-tag
  ````Bind many bindings at once in the same scope, adding the provided tag.
````
  [scope tag & body]

  (when (not (= (% (length body) 2) 0))
    (error "key/bind-many-tag requires an even number of arguments"))

  (as?-> body _
         (length _)
         (range 0 _ 2)
         (map |(tuple ;(array/slice body $ (+ $ 2))) _)
         (map |(do
                 (def [binding func] $)
                 (tuple 'key/bind scope binding func :tag tag)) _)))

(key/action
  action/command-palette
  "Open the command palette."
  (def binds (key/current))
  (def bound-actions
    (map
      |(do
         (def [desc func] $)
         (def sequence (as?-> binds x
                              (find |(= func ($ :function)) x)
                              (get x :sequence)
                              (string/join x " ")
                              (string " " x " ")))
         (tuple [desc (string sequence)] func))
      actions))

  (as?-> bound-actions _
         (input/find _
                     :full true
                     :reverse true
                     :prompt "search: actions")
         (apply _)))

(defn
  shell/new
  ```Create a new shell initialized in the working directory `path`.```
  [&opt path]
  (default path "")
  (def shells (group/mkdir :root "/shells"))
  (cmd/new shells :path path))

(defn
  shell/attach
  ```Create a new shell initialized in the working directory `path` and attach to it.```
  [&opt path]
  (default path "")
  (def shells (group/mkdir :root "/shells"))
  (pane/attach (cmd/new shells :path path)))

(key/action
  action/new-shell
  "Create a new shell."
  (def [ok current-path] (protect (cmd/path (pane/current))))
  (def path (if ok current-path (os/getenv "HOME" "")))
  (def shells (group/mkdir :root "/shells"))
  (def shell (cmd/new shells :path path :name (path/base path)))
  (pane/attach shell))

(key/action
  action/new-project
  "Create a new project."
  (def path (cmd/path (pane/current)))
  (def projects (group/mkdir :root "/projects"))
  (def project (group/new projects :name (path/base path)))
  (def editor
    (cmd/new project
             :path path
             :name "editor"
             :restart true
             :command (os/getenv "EDITOR" "vim")))
  (def shell
    (cmd/new project
             :path path
             :name "shell"
             :restart true))
  (pane/attach editor))

(key/action
  action/jump-project
  "Jump to a project."
  (def projects (group/mkdir :root "/projects"))
  (as?-> projects _
         (group/children _)
         (map
           |(tuple
              (tree/name $)
              {:type :node
               :id ((group/children $) 0)}
              $)
           _)
         (input/find _ :prompt "search: project")
         (group/children _)
         (_ 0) # Gets the first index, the editor
         (pane/attach _)))

(key/action
  action/jump-shell
  "Jump to a shell."
  (def shells (group/mkdir :root "/shells"))
  (as?-> (group/children shells) _
         (map |(tuple
                 (cmd/path $)
                 {:type :node
                  :id $}
                 $) _)
         (input/find _ :prompt "search: shell")
         (pane/attach _)))

(key/action
  action/next-pane
  "Move to the next pane."
  (def children
    (-?>>
      (pane/current)
      (tree/parent)
      (group/children)
      (filter tree/pane?)))

  (when (nil? children) (break))

  (def index (index-of (pane/current) children))

  (def next-panes
    (array/concat
      (array)
      (array/slice children (+ index 1))
      (array/slice children 0 index)))

  (when (= 0 (length next-panes)) (break))
  (def [next] next-panes)
  (pane/attach next))

(key/action
  action/rename-pane
  "Rename the current pane."
  (def pane (pane/current))
  (def old-path (tree/path pane))
  (as?-> pane _
         (input/text (string "rename: " (tree/path pane))
                     :preset (tree/name pane))
         (do (tree/set-name pane _) _)
         (msg/toast :info (string "renamed " old-path " to " (tree/path pane)))))

(key/action
  action/jump-pane
  "Jump to a pane."
  (as?-> (group/leaves :root) _
         (map |(tuple (tree/path $) {:type :node :id $} $) _)
         (input/find _ :prompt "search: pane")
         (pane/attach _)))

(key/action
  action/jump-screen-lines
  "Jump to a pane based on screen lines."
  (as?-> (group/leaves :root) _
         (mapcat
           (fn [id]
             (->> id
                  (pane/screen)
                  (filter (complement |(string/check-set " " $)))
                  (map
                    |(tuple [$ (tree/path id)] {:type :node :id id} id))))
           _)
         (input/find _ :prompt "search: screen")
         (pane/attach _)))

(key/action
  action/kill-server
  "Kill the cy server."
  (cy/kill-server))

(key/action
  action/detach
  "Detach from the cy server."
  (cy/detach))

(key/action
  action/choose-frame
  "Choose a frame."
  (as?-> (viewport/get-frames) _
         (map |(tuple $ {:type :frame :name $} $) _)
         (input/find _ :prompt "search: frame")
         (viewport/set-frame _)))

(key/action
  action/browse-animations
  "Browse animations."
  (as?-> (viewport/get-animations) _
         (map |(tuple $ {:type :animation :name $} $) _)
         (input/find _
                     # so we don't confuse the user
                     :animated false
                     :prompt "search: animation")))

(key/action
  action/reload-config
  "Reload the cy configuration."
  (cy/reload-config))

(key/action
  action/random-frame
  "Switch to a random frame."
  (def frames (viewport/get-frames))
  (def rng (math/rng))
  (viewport/set-frame (get frames (math/rng-int rng (length frames)))))

(key/action
  action/open-log
  "Open a .borg file."
  (as?-> (path/glob (path/join [(param/get :data-directory) "*.borg"])) _
         (map |(tuple $ {:type :replay :path $} $) _)
         (input/find _ :prompt "search: log file")
         (replay/open-file :root _)
         (pane/attach _)))

(defn- get-pane-commands [id result-func]
  (var [ok commands] (protect (cmd/commands id)))
  (if (not ok) (set commands @[]))
  (default commands @[])
  (map |(tuple [(string/replace-all "\n" "↵" ($ :text)) (tree/path id)]
               {:type :scrollback
                :focus ((($ :input) 0) :from)
                :highlights @[(($ :input) 0)]
                :id id}
               (result-func $)) commands))

(key/action
  action/jump-pane-command
  "Jump to a pane based on a command."
  (as?-> (group/leaves :root) _
         (mapcat |(get-pane-commands $ (fn [cmd] $)) _)
         (input/find _ :prompt "search: pane (command)")
         (pane/attach _)))

(key/action
  action/jump-command
  "Jump to the output of a command."
  (as?-> (group/leaves :root) _
         (mapcat |(get-pane-commands $ (fn [cmd] [$ cmd])) _)
         (input/find _ :prompt "search: command")
         (let [[id cmd] _]
           (pane/attach id)
           (replay/open
             id
             :main true
             :location (((cmd :input) 0) :from)))))

(key/action
  action/open-replay
  "Enter replay mode for the current pane."
  (replay/open (pane/current)))
