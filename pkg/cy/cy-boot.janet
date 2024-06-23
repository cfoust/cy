(def- prefix "ctrl+a")

(def projects (group/new :root :name "projects"))
(def shells (group/new :root :name "shells"))

(def- actions @[])

(defmacro key/action
  ````Register an action. Equivalent to the Janet built-in `(defn`), but requires a docstring.

An action is just a Janet function that is registered to the cy server with a short human-readable string description. It provides a convenient method for making some functionality you use often more discoverable.

In a similar way to other modern applications, cy has a command palette (invoked by default with `ctrl+a` `ctrl+p`, see [`(action/command-palette)
`](api.md#actioncommand-palette)) in which all registered actions will appear.
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
  "open command palette"
  (def binds (key/current))
  (as?-> actions _
         (map
           |(do
              (def [desc func] $)
              (def sequence (as?-> binds x
                                   (find |(= func ($ :function)) x)
                                   (get x :sequence)
                                   (string/join x " ")
                                   (string " " x " ")))
              (tuple [desc
                      (string
                        # Padding on the left prevents the table library from
                        # shrinking the key sequence string
                        (string/repeat " " (max (- 15 (length (string sequence))) 0))
                        sequence)] func))
           _)
         (input/find _ :prompt "search: actions")
         (apply _)))

(defn
  shell/new
  ```Create a new shell initialized in the working directory `path`.```
  [&opt path]
  (default path "")
  (cmd/new shells :path path))

(defn
  shell/attach
  ```Create a new shell initialized in the working directory `path` and attach to it.```
  [&opt path]
  (default path "")
  (pane/attach (cmd/new shells :path path)))

(key/action
  action/new-shell
  "create a new shell"
  (def path (cmd/path (pane/current)))
  (def shell (cmd/new shells :path path :name (path/base path)))
  (pane/attach shell))

(key/action
  action/new-project
  "create a new project"
  (def path (cmd/path (pane/current)))
  (def project (group/new projects :name (path/base path)))
  (def editor
    (cmd/new project
             :path path
             :name "editor"
             :command (os/getenv "EDITOR" "vim")))
  (def shell (cmd/new project :path path :name "shell"))
  (pane/attach editor))

(key/action
  action/jump-project
  "jump to a project"
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
  "jump to a shell"
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
  "move to the next pane"
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
  action/jump-pane
  "jump to a pane"
  (as?-> (group/leaves :root) _
         (map |(tuple (tree/path $) {:type :node :id $} $) _)
         (input/find _ :prompt "search: pane")
         (pane/attach _)))

(key/action
  action/jump-screen-lines
  "jump to a pane based on screen lines"
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
  action/kill-current-pane
  "kill the current pane"
  (tree/kill (pane/current)))

(key/action
  action/toggle-margins
  "toggle margins"
  (def size (viewport/size))
  (case (+ (size 0) (size 1))
    0 (viewport/set-size [0 80])
    (viewport/set-size [0 0])))

(key/action
  action/margins-80
  "set size to 80 columns"
  (viewport/set-size [0 80]))

(key/action
  action/margins-160
  "set size to 160 columns"
  (viewport/set-size [0 160]))

(key/action
  action/choose-frame
  "choose a frame"
  (as?-> (viewport/get-frames) _
         (input/find _ :prompt "search: frame")
         (viewport/set-frame _)))

(key/action
  action/reload-config
  "reload the cy configuration"
  (cy/reload-config))

(key/action
  action/random-frame
  "switch to a random frame"
  (def frames (viewport/get-frames))
  (def rng (math/rng))
  (viewport/set-frame (get frames (math/rng-int rng (length frames)))))

(key/action
  action/margins-smaller
  "decrease margins by 5 columns"
  (def [lines cols] (viewport/size))
  (viewport/set-size [lines (+ cols 10)]))

(key/action
  action/margins-bigger
  "increase margins by 5 columns"
  (def [lines cols] (viewport/size))
  (viewport/set-size [lines (- cols 10)]))

(key/action
  action/open-log
  "open a .borg file"
  (as?-> (path/glob (path/join [(cy/get :data-dir) "*.borg"])) _
         (map |(tuple $ {:type :replay :path $} $) _)
         (input/find _ :prompt "search: log file")
         (replay/open :root _)
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
  "jump to a pane based on a command"
  (as?-> (group/leaves :root) _
         (mapcat |(get-pane-commands $ (fn [cmd] $)) _)
         (input/find _ :prompt "search: pane (command)")
         (pane/attach _)))

(key/action
  action/jump-command
  "jump to a command"
  (as?-> (group/leaves :root) _
         (mapcat |(get-pane-commands $ (fn [cmd] [$ cmd])) _)
         (input/find _ :prompt "search: command")
         (let [[id cmd] _]
           (pane/attach id)
           (cy/replay
             :main true
             :location (((cmd :input) 0) :from)))))

(key/bind-many-tag :root "general"
                   [prefix "j"] action/new-shell
                   [prefix "n"] action/new-project
                   [prefix "k"] action/jump-project
                   [prefix "l"] action/jump-shell
                   ["ctrl+l"] action/next-pane
                   [prefix ";"] action/jump-pane
                   [prefix "c"] action/jump-pane-command
                   [prefix "C"] action/jump-command
                   [prefix ":"] action/jump-screen-lines
                   [prefix "ctrl+p"] action/command-palette
                   [prefix "x"] action/kill-current-pane
                   [prefix "g"] action/toggle-margins
                   [prefix "1"] action/margins-80
                   [prefix "2"] action/margins-160
                   [prefix "+"] action/margins-smaller
                   [prefix "-"] action/margins-bigger
                   [prefix "q"] cy/kill-server
                   [prefix "d"] cy/detach
                   [prefix "p"] cy/replay
                   [prefix "r"] action/reload-config
                   [prefix "P"] cy/paste)

(key/bind-many :time
               ["q"] replay/quit
               ["ctrl+c"] replay/quit
               ["esc"] replay/quit
               ["right"] replay/time-step-forward
               ["left"] replay/time-step-back
               ["/"] replay/search-forward
               ["?"] replay/search-backward
               ["g" "g"] replay/beginning
               ["n"] replay/search-again
               ["N"] replay/search-reverse
               [" "] replay/time-play
               ["1"] (fn [&] (replay/time-playback-rate 1))
               ["2"] (fn [&] (replay/time-playback-rate 2))
               ["3"] (fn [&] (replay/time-playback-rate 5))
               ["!"] (fn [&] (replay/time-playback-rate -1))
               ["@"] (fn [&] (replay/time-playback-rate -2))
               ["#"] (fn [&] (replay/time-playback-rate -5))
               ["G"] replay/end)

(key/bind-many :copy
               ["g" "g"] replay/beginning
               ["G"] replay/end
               ["/"] replay/search-forward
               ["?"] replay/search-backward
               ["q"] replay/quit
               ["ctrl+c"] replay/quit
               ["esc"] replay/quit
               ["left"] replay/cursor-left
               ["l"] replay/cursor-right
               # ??? <BS> in vim actually goes across lines
               ["backspace"] replay/cursor-left
               ["right"] replay/cursor-right
               # ??? <space> in vim actually goes across lines
               [" "] replay/cursor-right
               ["h"] replay/cursor-left
               ["ctrl+h"] replay/cursor-left
               ["ctrl+u"] replay/half-page-up
               ["ctrl+d"] replay/half-page-down
               ["up"] replay/scroll-up
               ["down"] replay/scroll-down
               ["j"] replay/cursor-down
               ["k"] replay/cursor-up
               ["v"] replay/select
               ["y"] replay/copy
               ["n"] replay/search-again
               ["N"] replay/search-reverse
               ["s"] replay/swap-screen
               ["]" "c"] replay/command-forward
               ["[" "c"] replay/command-backward
               ["0"] replay/start-of-line
               ["home"] replay/start-of-line
               ["g" "M"] replay/middle-of-line
               ["$"] replay/end-of-line
               ["^"] replay/first-non-blank
               ["g" "_"] replay/last-non-blank
               ["g" "0"] replay/start-of-screen-line
               ["g" "home"] replay/start-of-screen-line
               ["g" "m"] replay/middle-of-screen-line
               ["g" "$"] replay/end-of-screen-line
               ["g" "^"] replay/first-non-blank-screen
               ["g" "end"] replay/last-non-blank-screen
               [";"] replay/jump-again
               [","] replay/jump-reverse
               ["f" [:re "."]] replay/jump-forward
               ["F" [:re "."]] replay/jump-backward
               ["t" [:re "."]] replay/jump-to-forward
               ["T" [:re "."]] replay/jump-to-backward)
