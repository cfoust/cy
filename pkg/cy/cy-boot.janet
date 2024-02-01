(def prefix "ctrl+a")

(def projects (group/new :root :name "projects"))
(def shells (group/new :root :name "shells"))

(def- actions @[])

(defmacro key/def
  "register an action"
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

(key/def
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
                                   (string/join x " ")))
              (tuple [desc (string sequence)] func))
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

(key/def
  action/new-shell
  "create a new shell"
  (def path (cmd/path (pane/current)))
  (def shell (cmd/new shells :path path :name (path/base path)))
  (pane/attach shell))

(key/def
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

(key/def
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

(key/def
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

(key/def
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

(key/def
  action/jump-pane
  "jump to a pane"
  (as?-> (group/leaves :root) _
         (map |(tuple (tree/path $) {:type :node :id $} $) _)
         (input/find _ :prompt "search: pane")
         (pane/attach _)))

(key/def
  action/kill-current-pane
  "kill the current pane"
  (tree/kill (pane/current)))

(key/def
  action/toggle-margins
  "toggle margins"
  (def size (viewport/size))
  (case (+ (size 0) (size 1))
    0 (viewport/set-size [0 80])
    (viewport/set-size [0 0])))

(key/def
  action/margins-80
  "set size to 80 columns"
  (viewport/set-size [0 80]))

(key/def
  action/margins-160
  "set size to 160 columns"
  (viewport/set-size [0 160]))

(key/def
  action/choose-frame
  "choose a frame"
  (as?-> (viewport/get-frames) _
         (input/find _ :prompt "search: frame")
         (viewport/set-frame _)))

(key/def
  action/random-frame
  "switch to a random frame"
  (def frames (viewport/get-frames))
  (def rng (math/rng))
  (viewport/set-frame (get frames (math/rng-int rng (length frames)))))

(key/def
  action/margins-smaller
  "decrease margins by 5 columns"
  (def [lines cols] (viewport/size))
  (viewport/set-size [lines (+ cols 10)]))

(key/def
  action/margins-bigger
  "increase margins by 5 columns"
  (def [lines cols] (viewport/size))
  (viewport/set-size [lines (- cols 10)]))

(key/def
  action/open-log
  "open an existing log file"
  (as?-> (path/glob (path/join [(cy/get :data-dir) "*.borg"])) _
         (map |(tuple $ {:type :replay :path $} $) _)
         (input/find _ :prompt "search: log file")
         (replay/open :root _)
         (pane/attach _)))

(key/bind-many :root
               [prefix "j"] action/new-shell
               [prefix "n"] action/new-project
               [prefix "k"] action/jump-project
               [prefix "l"] action/jump-shell
               ["ctrl+l"] action/next-pane
               [prefix ";"] action/jump-pane
               [prefix "ctrl+p"] action/command-palette
               [prefix "x"] action/kill-current-pane
               [prefix "g"] action/toggle-margins
               [prefix "1"] action/margins-80
               [prefix "2"] action/margins-160
               [prefix "+"] action/margins-smaller
               [prefix "-"] action/margins-bigger
               [prefix "r" "r"] action/random-frame
               [prefix "q"] cy/kill-server
               [prefix "d"] cy/detach
               [prefix "p"] cy/replay
               [prefix "P"] cy/paste)

(key/bind-many :replay
               ["q"] replay/quit
               ["ctrl+c"] replay/quit
               ["esc"] replay/quit
               ["right"] replay/time-step-forward
               ["left"] replay/time-step-back
               ["up"] replay/scroll-up
               ["down"] replay/scroll-down
               ["ctrl+u"] replay/half-page-up
               ["ctrl+d"] replay/half-page-down
               ["/"] replay/search-forward
               ["?"] replay/search-backward
               ["g" "g"] replay/beginning
               ["G"] replay/end
               ["l"] replay/cursor-right
               ["h"] replay/cursor-left
               ["j"] replay/cursor-down
               ["k"] replay/cursor-up
               ["v"] replay/select
               ["y"] replay/copy
               ["n"] replay/search-again
               ["N"] replay/search-reverse
               [" "] replay/time-play
               ["1"] (fn [&] (replay/time-playback-rate 1))
               ["2"] (fn [&] (replay/time-playback-rate 2))
               ["3"] (fn [&] (replay/time-playback-rate 5))
               ["!"] (fn [&] (replay/time-playback-rate -1))
               ["@"] (fn [&] (replay/time-playback-rate -2))
               ["#"] (fn [&] (replay/time-playback-rate -5))
               [";"] replay/jump-again
               [","] replay/jump-reverse
               ["f" [:re "."]] replay/jump-forward
               ["F" [:re "."]] replay/jump-backward
               ["t" [:re "."]] replay/jump-to-forward
               ["T" [:re "."]] replay/jump-to-backward)


(key/def
  action/current
  "print stuff"
  (pp (key/current))
  (pp (key/get :root)))
