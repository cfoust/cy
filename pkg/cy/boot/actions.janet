(def- actions @{})

(defn-
  key/register-action
  "Register an action."
  [name docstring func]
  (put actions name [docstring func]))

(defmacro key/action
  ````Register an action. Equivalent to the Janet built-in `(defn)`, but requires a docstring.

An action is just a Janet function that is registered to the cy server with a short human-readable string description. It provides a convenient method for making some functionality you use often more discoverable.

In a similar way to other modern applications, cy has a command palette (invoked by default with {{bind :root ctrl+a ctrl+p}}, see {{api action/command-palette}}) in which all registered actions will appear.
````
  [name docstring & body]
  (def func-name (string name))
  ~(upscope
     (defn ,name ,docstring [] ,;body)
     (,key/register-action ,func-name ,docstring ,name)))

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

(defmacro param/set-many
  ````Set many params at once with the same target.

For example:
```janet
(param/set-many :root
               :replay-play-bg "#ff0000"
               :replay-time-bg "#00ff00")
```
````
  [target & body]

  (when (not (= (% (length body) 2) 0))
    (error "param/set-many requires an even number of arguments"))

  (as?-> body _
         (length _)
         (range 0 _ 2)
         (map |(tuple ;(array/slice body $ (+ $ 2))) _)
         (map |(do
                 (def [key value] $)
                 (tuple 'param/set target key value)) _)))

(defn
  key/get-actions
  "Get all registered actions."
  []
  (table/clone actions))

(key/action
  action/command-palette
  "Open the command palette."
  (def binds (key/current))
  (def bound-actions
    (map
      |(do
         (def [name [desc func]] $)
         (def sequence (as?-> binds x
                              (find |(= func ($ :function)) x)
                              (get x :sequence)
                              (string/join x " ")
                              (string " " x " ")))
         (tuple [(get (string/split "\n" desc) 0)
                 name
                 (string sequence)] func))
      (pairs actions)))

  (as?-> bound-actions _
         (input/find _
                     :full true
                     :reverse true
                     :prompt "search: actions")
         (apply _)))

(defn
  shell/new
  ```Create a new shell initialized in the working directory `path`. If `path` is not provided, this uses the path of the current pane.```
  [&opt path]
  (default path
    (do
      (def [ok current-path] (protect (cmd/path (pane/current))))
      (if ok current-path (os/getenv "HOME" ""))))
  (def shells (group/mkdir :root "/shells"))
  (cmd/new shells :path path))

(defn
  shell/attach
  ```Create a new shell initialized in the working directory `path` and attach to it.```
  [&opt path]
  (pane/attach (shell/new path)))

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
  # Remove existing projects for this path
  (->> (group/children projects)
       (filter |(= (path/base path) (tree/name $)))
       (map tree/rm))
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
  action/close-project
  "Close the current project."
  (def projects (group/mkdir :root "/projects"))
  (def id (pane/current))
  (if (nil? id) (break))
  # Walk up the tree to find the project group
  (var project nil)
  (var node id)
  (while (not (nil? node))
    (def [ok parent] (protect (tree/parent node)))
    (if (not ok) (break))
    (if (= projects parent)
      (do (set project node) (break)))
    (set node parent))
  (if (nil? project) (break))
  (tree/rm project))

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

(defn-
  get-siblings
  "Get the siblings of the current pane."
  []

  (def children
    (-?>>
      (pane/current)
      (tree/parent)
      (group/children)
      (filter tree/pane?)))

  (when (nil? children) (break))

  (def index (index-of (pane/current) children))
  (array/concat
    (array)
    (array/slice children (+ index 1))
    (array/slice children 0 index)))

(key/action
  action/next-pane
  "Move to the next sibling pane."
  (as?-> (get-siblings) _
         (get _ 0)
         (pane/attach _)))

(key/action
  action/prev-pane
  "Move to the previous sibling pane."
  (as?-> (get-siblings) _
         (reverse _)
         (get _ 0)
         (pane/attach _)))

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
  action/jump-group-pane
  "Jump to a pane that is a descendant of the current group."
  (as?-> (pane/current) _
         (tree/parent _)
         (group/leaves _)
         (map |(tuple (tree/path $) {:type :node :id $} $) _)
         (input/find _
                     :prompt (string
                               "search: panes in "
                               (->>
                                 (pane/current)
                                 (tree/parent)
                                 (tree/path))))
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
  (if (input/ok? "kill the cy server?")
    (cy/kill-server)))

(key/action
  action/detach
  "Detach from the cy server."
  (if (input/ok? "detach from the cy server?")
    (cy/detach)))

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
         (replay/open-file (group/mkdir :root "/borg") _)
         (pane/attach _)))

(defn- get-pane-commands [id result-func]
  (var [ok commands] (protect (cmd/commands id)))
  (if (not ok) (set commands @[]))
  (default commands @[])
  (map |(let [[index cmd] $]
          [[(string/replace-all "\n" "↵" (cmd :text)) (tree/path id)]
           {:type :scrollback
            :focus (((cmd :input) 0) :from)
            :highlights @[((cmd :input) 0)]
            :id id}
           (result-func index cmd)])
       (pairs commands)))

(key/action
  action/jump-pane-command
  "Jump to a pane based on a command."
  (as?-> (group/leaves :root) _
         (mapcat |(get-pane-commands $ (fn [index cmd] $)) _)
         (input/find _ :prompt "search: pane (command)")
         (pane/attach _)))

(key/action
  action/jump-command
  "Jump to the output of a command."
  (as?-> (group/leaves :root) _
         (mapcat |(get-pane-commands $ (fn [index cmd] [$ cmd])) _)
         (input/find _ :prompt "search: command")
         (let [[id {:input [{:from from}]}] _]
           (pane/attach id)
           (replay/open
             id
             :alt-screen false
             :focus from))))

(key/action
  action/recall-command
  "Recall the output of a command to the current shell."
  (as?-> (group/leaves :root) _
         (mapcat |(get-pane-commands $ (fn [index &] [$ index])) _)
         (input/find _ :prompt "search: command")
         (let [[node index] _]
           (def ref (if
                      (= node (pane/current)) (string index)
                      (string node ":" index)))
           (pane/send-keys (pane/current) @[(string "cy " ref)]))))

(key/action
  action/open-replay
  "Enter replay mode for the current pane."
  (replay/open (pane/current)))

(key/action
  action/cpu-profile
  "Save a CPU profile to cy's socket directory."
  (cy/cpu-profile))

(key/action
  action/memory-profile
  "Save a memory profile to cy's socket directory."
  (cy/memory-profile))

(key/action
  action/trace
  "Save a trace to cy's socket directory."
  (cy/trace))

(key/action
  action/search-borg
  "Search all recorded .borg files for a pattern."
  (as?-> (input/text "search: recorded sessions") _
         (search/new (group/mkdir :root "/search") _)
         (pane/attach _)))

(def- ctrlr-input-options
  [:height 10
   :full true
   :animated false
   :headers ["command" "timestamp" "directory" "source"]])

(defn- ctrl-r-get-db-commands []
  (def format (param/get :timestamp-format))
  (map |(let [{:borg borg
               :command {:text text
                         :directory cwd
                         :executed-at ts
                         :input input}} $
              {:command cmd} $]
          [[(string/replace-all "\n" "↵" text)
            (time/format ts format)
            cwd
            "db"]
           {:type :replay
            :path borg
            :focus ((input 0) :from)
            :highlights @[(input 0)]
            :alt-screen false}
           $]) (cmd/query)))

(defn- ctrl-r-get-pane-commands [id]
  (def format (param/get :timestamp-format))
  (var [ok commands] (protect (cmd/commands id)))
  (if (not ok) (set commands @[]))
  (default commands @[])
  (map |(let [{:text text
               :input input
               :directory cwd
               :executed-at ts} $]
          [[(string/replace-all "\n" "↵" text)
            (time/format ts format)
            cwd
            "pane"]
           {:type :scrollback
            :focus ((input 0) :from)
            :highlights @[(input 0)]
            :id id}
           {:command $}])
       (reverse commands)))

(key/action
  action/ctrl-r
  "Find a recent command and insert it into the current shell."
  (as?-> (array/concat @[]
                       (ctrl-r-get-pane-commands (pane/current))
                       (ctrl-r-get-db-commands)) _
         (input/find _ :prompt "search: ctrl-r"
                     ;ctrlr-input-options)
         (let [{:command {:text text}} _]
           (pane/send-keys (pane/current) @[text]))))

(key/action
  action/jump-history-command
  "Find a command and open its .borg file."
  (as?-> (ctrl-r-get-db-commands) _
         (input/find _ :prompt "search: command (borg)"
                     ;ctrlr-input-options)
         (let [{:borg borg
                :command {:input [{:from input}]}} _]
           (replay/open-file
             (group/mkdir :root "/borg")
             borg
             :focus input
             :alt-screen false))
         (pane/attach _)))

(key/action
  action/insert-thumb
  "Select a thumb and insert it into the current pane."
  (var choice (input/thumbs))
  (when (nil? choice) (break))
  (pane/send-text (pane/current) choice))

(merge-module root-env (curenv))
